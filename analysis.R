#################################################################################################################################################################
# Read, Q. D., et al. 2019. Assessing the environmental impacts of halving food loss and waste along the food supply chain. Science of the Total Environment.   #
#                                                                                                                                                               #
# Code to reproduce all analyses in manuscript, beginning with cleaned data                                                                                     #
# Last modified by QDR on 07 January 2020                                                                                                                       #
# Tested with R version 3.6.2 on Windows and Linux                                                                                                              #                                                                                                   
# Contact: qread@sesync.org                                                                                                                                     #
#################################################################################################################################################################


# Here, specify whether you want to run in parallel with multiple cores
# Note that Windows does not support parallel execution with mcmapply()
# Default is 1 core (not parallel) but you can increase the number.
n_cores <- 1 

halvingfoodwaste_path <- getwd()

#########################################  
# Load packages needed to run analysis  #
#########################################

library(tidyverse)
library(reticulate)
library(parallel)

########################
# Load raw data into R #
########################

# Food loss and waste rates by food type and food supply chain stage
flw_rates <- read.csv('data/flw_rates.csv', stringsAsFactors = FALSE)

# BEA industries from input-output table with the proportion of each industry assigned to the food supply chain,
# and within each industry the proportion assigned to each food commodity group
industry_proportions <- read.csv('data/industry_proportions.csv', stringsAsFactors = FALSE) %>%
  filter(food_system %in% c('partial', 'y')) %>%
  arrange(stage) %>%
  mutate(stage_code = case_when(
    stage %in% 'agriculture' ~ 'L1',
    stage %in% 'processing' ~ 'L2',
    stage %in% c('retail', 'wholesale') ~ 'L3',
    stage %in% 'foodservice' ~ 'L4a',
    stage %in% 'institutional' ~ 'L4b'
  ))

# Make and use tables used to build the EEIO model, with 2012 values mapped to the 2007 BEA schema
M <- read.csv('data/make2012.csv', row.names = 1, check.names = FALSE)
U <- read.csv('data/use2012.csv', row.names = 1, check.names = FALSE)

# Table to correct the formatting of the BEA codes
bea_code_formats <- read.csv('data/industry_codes.csv', stringsAsFactors = FALSE)

#########################################  
# Source scripts needed to run analysis #
#########################################

# To reproduce this analysis, it is necessary to use a modified version of the USEEIO model (included with this repo)
# Source R script used to build USEEIO model for each scenario.
source('USEEIO/R/Model Build Scripts/USEEIO2012_buildfunction.R')
model_build_path <- file.path(halvingfoodwaste_path, 'USEEIO/useeiopy/Model Builds')

# Source other R scripts needed to carry out analysis.
source('code/R_functions.R')

# Source Python script to call the built EEIO models can be called from R (note: this changes working directory)
source_python('code/eeio_lcia.py')

############################################################################
# Create vectors of industry names and industry codes in different formats #
############################################################################

sector_stage_codes <- industry_proportions$stage_code
sector_long_names <- bea_code_formats$sector_desc_drc[match(industry_proportions$BEA_389_code, bea_code_formats$sector_code_uppercase)]
sector_short_names <- industry_proportions$BEA_389_code
final_demand_sector_codes <- sector_stage_codes
final_demand_sector_codes[final_demand_sector_codes %in% c('L1', 'L2', 'L3')] <- 'L5'

#################################################################################################### 
# Calculate 0%, 50% and 100% FLW reduction environmental impact values for each supply chain stage #
####################################################################################################

# Get baseline waste rate for each industry using the stage it belongs to and weighted average of the food categories in that industry
# Original version has equal proportions of all relevant food categories in each industry
# Later will be replaced with a correct weighted average
flw_rates <- flw_rates %>%
  mutate(L1 = loss_ag_production,
         L2 = 1 - (1 - loss_handling_storage) * (1 - loss_processing_packaging),
         L3 = loss_distribution,
         L4a = loss_consumption,
         L4b = loss_consumption)

waste_rate_bysector <- t(flw_rates[, industry_proportions$stage_code])
food_category_weights <- industry_proportions %>% select(cereals:beverages)
baseline_waste_rate <- rowSums(waste_rate_bysector * food_category_weights, na.rm = TRUE) / rowSums(food_category_weights)

# Function to get change in demand given original waste rate W0, reduction rate r, and proportion of the industry's output that is food p
demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1

# Use 5 levels of reduction for each stage. 0%, 50%, 100%
# 3 levels of reduction in 6 stages = 3^6 scenarios = 729

rate_levels <- c(0, 0.5, 1)
reduction_rate_grid <- expand.grid(L1 = rate_levels, L2 = rate_levels, L3 = rate_levels, L4a = rate_levels, L4b = rate_levels, L5 = rate_levels)

# Create list from grid
reduction_rate_grid_list <- setNames(split(reduction_rate_grid, seq(nrow(reduction_rate_grid))), rownames(reduction_rate_grid))

# Run analysis either serially or in parallel depending on value of n_cores
eeio_result_grid <- mcmapply(get_reduction, reduction_by_stage = reduction_rate_grid_list, scenario_id = 1:length(reduction_rate_grid_list), mc.cores = n_cores, SIMPLIFY = FALSE)

eeio_result_grid_df <- bind_rows(eeio_result_grid)

# Put output into data frame and write to CSV.
eeio_result_grid_df <- cbind(reduction_rate_grid[rep(1:nrow(reduction_rate_grid), each = 21), ], eeio_result_grid_df)

write.csv(eeio_result_grid_df, file = file.path(halvingfoodwaste_path, 'output/sixstage_scenario_grid_lcia_results.csv'), row.names = FALSE)

###########################
# Do uncertainty analysis #
###########################

# Function to generate a sampling draw for proportion food, if it's less than 1.
# Accepts input of p (vector) and factor for uncertainty of beta distribution. So each is beta(fp, f(1-p))
proportion_food_draw <- function(p, f = 100) rbeta(n = length(p), shape1 = f * p, shape2 = f * (1 - p))

# Function to generate a sampling draw for the baseline waste rates.
# Assume no relationship among the various loss rates, whether within category or within stage
# This could be modified by replacing beta distributions with Dirichlet distributions
# Accepts input of w, which is a matrix of the loss rates, and f, factor for uncertainty of beta distribution
baseline_rate_table_draw <- function(w, f = 100) {
  res <- apply(w, 2, function(p) rbeta(n = length(p), shape1 = f * p, shape2 = f * (1 - p)))
  res[!is.finite(res)] <- NA
  res
}

n_draws <- 100 # Number of rounds of uncertainty analysis
uncertainty_factor <- 100 # The higher this number, the LOWER the uncertainty.
set.seed(333)

# Only use 0% reduction (baseline) and 50% reduction for each stage
# Then do 0 and 100% separately

reduction_rate_grid <- rbind(expand.grid(replicate(6, c(0, 0.5), simplify = FALSE)),
							 expand.grid(replicate(6, c(0, 1), simplify = FALSE))[-1,])
names(reduction_rate_grid) <- c('L1', 'L2', 'L3', 'L4a', 'L4b', 'L5')

# Create list from grid
reduction_rate_grid_list <- split(reduction_rate_grid, seq(nrow(reduction_rate_grid))) %>%
  map(unlist)

# Take random draws around mean for FAO waste rate
flw_rates_list <- replicate(n_draws, baseline_rate_table_draw(w = flw_rates %>% select(loss_ag_production:loss_consumption), f = uncertainty_factor), simplify = FALSE)

# Get L1 - L4b values from each draw
flw_rates_list <- map(flw_rates_list, ~ .x %>%
                        as.data.frame %>%
                        mutate(L1 = loss_ag_production,
                               L2 = 1 - (1 - loss_handling_storage) * (1 - loss_processing_packaging),
                               L3 = loss_distribution,
                               L4a = loss_consumption,
                               L4b = loss_consumption))

# Take random draws around mean for proportion food in each sector
proportion_food_list <- replicate(n_draws, proportion_food_draw(p = industry_proportions$proportion_food, f = uncertainty_factor), simplify = FALSE)

waste_rate_bysector_list <- map(flw_rates_list, ~ t(.x[, industry_proportions$stage_code]))
food_category_weights <- industry_proportions %>% select(cereals:beverages)
baseline_waste_rate_list <- map(waste_rate_bysector_list, ~ rowSums(.x * food_category_weights, na.rm = TRUE) / rowSums(food_category_weights))

# Cross the lists of reduction rates with the different parameters
# Total number of models to run: number of draws * number of reduction rate combinations
baseline_waste_rate_combos <- cross2(reduction_rate_grid_list, baseline_waste_rate_list)
proportion_food_combos <- cross2(reduction_rate_grid_list, proportion_food_list)
																						 
uncertainty_arguments <- list(reduction_rate = map(baseline_waste_rate_combos, 1),
                              baseline_waste_rate = map(baseline_waste_rate_combos, 2),
                              proportion_food = map(proportion_food_combos, 2),
                              scenario_id = as.list(paste0('scenario', 1:length(baseline_waste_rate_combos))))


# Run uncertainty analysis, either serially or in parallel.
eeio_result_grid_uncertainty <- with(uncertainty_arguments, mcmapply(get_reduction_from_list, reduction_rate, baseline_waste_rate, proportion_food, scenario_id, mc.cores = n_cores, SIMPLIFY = FALSE))

# Match output with arguments to get confidence intervals
grid_uncertainty_df <- map2_dfr(uncertainty_arguments$reduction_rate, eeio_result_grid_uncertainty, ~ data.frame(t(.x), .y))

grid_uncertainty_CIs <- grid_uncertainty_df %>%
  group_by(L1, L2, L3, L4a, L4b, L5, impact_category) %>%
  do(quantile(.$value, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) %>% t %>% as.data.frame %>% setNames(c('q025', 'q25', 'q50', 'q75', 'q975')))

# Write confidence intervals to CSV
write.csv(grid_uncertainty_CIs, file.path(halvingfoodwaste_path, 'output/uncertainty_grid_CIs.csv'), row.names = FALSE)

################################################################################
# Calculate environmental impact reductions for food-specific waste reductions #
################################################################################

# Get waste rates by sector x commodity
waste_rate_bycommodityxsector <- waste_rate_bysector * food_category_weights

# Specify environmental impact categories and food commodity groups to minimize

# Environmental goods to minimize
categories <- c("impact potential/gcc/kg co2 eq", "resource use/land/m2*yr", "resource use/watr/m3", "resource use/enrg/mj", "impact potential/eutr/kg n eq")

# Commodities to target
commodities <- names(food_category_weights)

# All combinations of category x commodity
scenarios <- expand.grid(category = categories, commodity = commodities, stringsAsFactors = FALSE)

# Run across all combinations, either serially or in parallel.
results_by_commodity <- mcmapply(find_best_pathway, category = scenarios$category, commodity = scenarios$commodity, mc.cores = n_cores, SIMPLIFY = FALSE)

results_by_commodity <- tibble(category = scenarios$category, 
                               commodity = scenarios$commodity, 
                               best_pathway = map(results_by_commodity, 'best_pathway'), 
                               impacts = map(results_by_commodity, 'best_pathway_impacts'))

# Get baseline values to normalize results
baseline_result <- eeio_result_grid_df %>%
  filter(scenario == 1)
  
# Convert list columns of tibble into regular columns
# Keep only the environmental good being minimized
results_by_commodity_df <- results_by_commodity %>% 
  pmap_dfr(function(category, commodity, best_pathway, impacts) data.frame(category = category, 
                                                                           commodity = commodity,
                                                                           n_stages_reduced = 1:length(best_pathway),
                                                                           stage_reduced = best_pathway,
                                                                           impact = map_dbl(impacts, ~ .x$value[.x$impact_category == category]))) %>%
  left_join(baseline_result %>% select(impact_category, value), by = c('category'='impact_category')) %>% 
  rename(baseline_impact = value) %>%
  mutate(impact_norm = impact / baseline_impact)
  
write.csv(results_by_commodity_df, file.path(halvingfoodwaste_path, 'output/bestpathway_bycommodity.csv'), row.names = FALSE)
