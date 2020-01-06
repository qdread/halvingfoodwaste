# ===================
# get_eeio_result() =
# ===================

# Function to build the USEEIO model with modified intermediate and final demand values and return the result.
# Takes several inputs: 
# c_factor is the factor to multiply the intermediate column inputs (vector), 
# r_factor is the factor to multiply the rows of the final demand column (vector),   
# c_names is the names of the columns to multiply by c_factor (vector with same length as c_factor)
# r_names is the names of the rows to multiply by r_factor (vector with same length as r_factor)
# i is the name of the scenario
# By default the only final demand column we modify is F01000: personal consumption expenditures (PCE)

get_eeio_result <- function(c_factor, r_factor, c_names, r_names, i = 'no_name') {

  # Step 3. Build USEEIO with specifications for modifying intermediate and final demand.
  build_USEEIO(outputfolder = file.path(model_build_path, paste0('scenario_', i)),
               model = paste0('scenario_', i),
               usetablefile = file.path(halvingfoodwaste_path, 'data/use2012.csv'),
               maketablefile = file.path(halvingfoodwaste_path, 'data/make2012.csv'),
               code_path = file.path(halvingfoodwaste_path, 'USEEIO'),
               intermediate_columns_modify = c_names,
               intermediate_change_factor = c_factor,
               final_rows_modify = r_names,
               final_columns_modify = c('F01000'),
               final_change_factor = r_factor
  )
  # Step 4. Extract demand vector for food system from scenario.
  # Join this with the food system proportions and with the correct demand codes (full description names)
  all_final_demand <- read.csv(file.path(model_build_path, paste0('scenario_', i), paste0('scenario_', i, '_FinalDemand.csv')), stringsAsFactors = FALSE) %>%
    left_join(industry_proportions, by = c('BEA_389_code', 'BEA_389_def')) %>% 
    filter(!is.na(proportion_food)) %>%
    left_join(bea_code_formats, by = c('BEA_389_code' = 'sector_code_uppercase'))
  # Convert demand vector to separate list of codes and values
  final_demand_list <- with(all_final_demand, list(codes = as.list(sector_desc_drc), values = as.list(X2012_US_Consumption * proportion_food)))
  # Step 5. Run the model!
  eeio_result <- eeio_lcia(paste0('scenario_', i), final_demand_list$values, final_demand_list$codes) 
  eeio_result <- data.frame(scenario = i, impact_category = rownames(eeio_result), value = eeio_result$Total, stringsAsFactors = FALSE)
  # Step 6. Delete the intermediate files (entire model build folder) 
  unlink(file.path(model_build_path, paste0('scenario_', i)), recursive = TRUE)
  
  return(eeio_result)
}

# =================
# get_reduction() =
# =================

# Wrapper function so that get_eeio_result() is compatible with parallel::mcmapply()

get_reduction <- function(reduction_by_stage, scenario_id) {
  intermediate_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[sector_stage_codes], industry_proportions$proportion_food))
  final_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[final_demand_sector_codes], industry_proportions$proportion_food))
  get_eeio_result(c_factor = intermediate_demand_change_factors,
                  c_names = sector_long_names,
                  r_factor = final_demand_change_factors,
                  r_names = sector_short_names,
                  i = scenario_id)
}

# ===============================
# get_eeio_result_uncertainty() =
# ===============================

# Function to get results from EEIO with different values from the uncertainty analysis.

get_eeio_result_uncertainty <- function(c_factor, r_factor, c_names, r_names, i = 'no_name', crosswalk = industry_proportions) {
  
  # Build USEEIO with specifications for modifying intermediate and final demand.
  build_USEEIO(outputfolder = file.path(model_build_path, paste0('scenario_', i)),
               model = paste0('scenario_', i),
               usetablefile = file.path(halvingfoodwaste_path, 'data/use2012.csv'),
               maketablefile = file.path(halvingfoodwaste_path, 'data/make2012.csv'),
               code_path = file.path(halvingfoodwaste_path, 'USEEIO'),
               intermediate_columns_modify = c_names,
               intermediate_change_factor = c_factor,
               final_rows_modify = r_names,
               final_columns_modify = c('F01000'),
               final_change_factor = r_factor
  )
  # Extract demand vector for food system from scenario.
  # Join this with the food system proportions and with the correct demand codes (full description names)
  all_final_demand <- read.csv(file.path(model_build_path, paste0('scenario_', i), paste0('scenario_', i, '_FinalDemand.csv')), stringsAsFactors = FALSE) %>%
    left_join(crosswalk, by = c('BEA_389_code', 'BEA_389_def')) %>% 
    filter(!is.na(proportion_food)) %>%
    left_join(bea_code_formats, by = c('BEA_389_code' = 'sector_code_uppercase'))
  # Convert demand vector to separate list of codes and values
  final_demand_list <- with(all_final_demand, list(codes = as.list(sector_desc_drc), values = as.list(X2012_US_Consumption * proportion_food)))
  # Step 5. Run the model!
  eeio_result <- eeio_lcia(paste0('scenario_', i), final_demand_list$values, final_demand_list$codes) 
  eeio_result <- data.frame(scenario = i, impact_category = rownames(eeio_result), value = eeio_result$Total, stringsAsFactors = FALSE)
  # Step 6. Delete the intermediate files (entire model build folder) 
  unlink(file.path(model_build_path, paste0('scenario_', i)), recursive = TRUE)
  
  return(eeio_result)
}

# =============================
# get_reduction_uncertainty() =
# =============================

# Wrapper function so that get_eeio_result_uncertainty() is compatible with parallel::mcmapply()

get_reduction_from_list <- function(reduction_rate, baseline_waste_rate, proportion_food, scenario_id) {
  intermediate_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_rate[sector_stage_codes], proportion_food))
  final_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_rate[final_demand_sector_codes], proportion_food))
  get_eeio_result_uncertainty(c_factor = intermediate_demand_change_factors,
                              c_names = sector_long_names,
                              r_factor = final_demand_change_factors,
                              r_names = sector_short_names,
                              i = scenario_id,
                              crosswalk = industry_proportions %>% mutate(proportion_food = !!proportion_food))
}

# =====================
# evaluate_scenario() =
# =====================

# Wrapper function to evaluate USEEIO for a single scenario, specifying the waste rate for the food commodity group we want.
evaluate_scenario <- function(reduction_by_stage, baseline_waste_rate, scenario_id) {
  intermediate_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[sector_stage_codes], industry_proportions$proportion_food))
  final_demand_change_factors <- as.numeric(demand_change_fn(baseline_waste_rate, reduction_by_stage[final_demand_sector_codes], industry_proportions$proportion_food))
  intermediate_demand_change_factors[is.na(intermediate_demand_change_factors)] <- 1
  final_demand_change_factors[is.na(final_demand_change_factors)] <- 1
  get_eeio_result(c_factor = intermediate_demand_change_factors,
                  c_names = sector_long_names,
                  r_factor = final_demand_change_factors,
                  r_names = sector_short_names,
                  i = scenario_id)
}

# =====================
# find_best_pathway() =
# =====================

# Function to find the sequence of 50% reductions of waste in a chosen food commodity group that minimizes the chosen environmental category

find_best_pathway <- function(category, commodity) {
  # Find the waste rates for the commodity being evaluated
  commodity_waste_rates <- waste_rate_bycommodityxsector[, commodity]
  
  # Find the stages across which we will optimize (many will have 6 but some that aren't in agriculture stage will only have 5)
  stages <- c('L2','L3','L4a','L4b','L5')
  if (!is.na(commodity_waste_rates[1])) stages <- c('L1', stages)
  
  # Initialize best pathway and other structures to hold data
  best_pathway <- c()
  best_pathway_impacts <- list()
  reductions <- setNames(rep(0, 6), c('L1', 'L2', 'L3', 'L4a', 'L4b', 'L5'))
  
  # Each time the iteration runs, we select one stage from the remaining not yet chosen stages and add it to the already chosen stages
  # Then run the scenario and see which stage minimizes the environmental impact in the chosen good
  # This is still done even when there is only one stage left since we want to know the impacts when the one final stage is reduced
  for (i in 1:length(stages)) {
    # Print a progress message
    message('Evaluating stage ', i, ' for ', commodity, ' minimizing ', category)
    
    # Define each of the remaining scenarios
    candidate_scenarios <- sapply(stages, function(x) {
      to_reduce <- c(best_pathway, x)
      reductions[to_reduce] <- 0.5
      return(reductions)
    }, simplify = FALSE)
    
    # Evaluate each scenario for the given commodity only (just serially, not in parallel, since there are at most 6)
    scenario_results <- map(candidate_scenarios, evaluate_scenario, baseline_waste_rate = commodity_waste_rates, scenario_id = paste(commodity, gsub('[^a-z]', '', category), i, sep = '_'))
    
    # Identify which scenario minimizes the environmental good in question, and append it to best_pathway
    impact_values <- map_dbl(scenario_results, ~ .x$value[.x$impact_category == category])
    best_stage <- which.min(impact_values)
    best_pathway <- c(best_pathway, stages[best_stage])
    
    # Remove it from the remaining candidate stages
    stages <- stages[!stages %in% best_pathway]
    
    # Write the best scenario results to the list of outputs
    best_pathway_impacts[[i]] <- scenario_results[[best_stage]]
  }
  
  # Return the results
  return(list(best_pathway = best_pathway, best_pathway_impacts = best_pathway_impacts))
}
