#Generate components of USEEIOv1.1 model
#currently produces:
# Final demand - total consumption and production vectors for 2012
# Direct requirements coefficients
# Market shares
# LCIA factors
#See the README in the model folder for info on preparation of other model components

# Version created by QDR 09 Nov 2018: Uses 2012 BEA input-output tables (older version used 2007)
# "Forked" version created by QDR 06 Dec 2018: Converted to a function so that any use table can be used
# This enables us to use the counterfactual use tables made for scenario analysis. (Must edit demand and io function source code too)
# Updated 18 April 2019: Counterfactual make table is also added. Underlying functions must be edited to include the correct make tables.

build_USEEIO <- function(outputfolder = paste(ModelBuildspath,"USEEIO2012/",sep=""), 
                         model = 'USEEIO2012', 
                         modelversion = 'v1.1', 
                         majormodelversion = 1, 
                         BEApath = 'Z:/BEA/formatted', 
                         Y = 2012, 
                         usetablefile = file.path(BEApath, 'use2012.csv'),
						 maketablefile = file.path(BEApath, 'make2012.csv'),
                         code_path = '.',
						 intermediate_columns_modify = character(0),
						 intermediate_change_factor = numeric(0),
						 final_rows_modify = character(0),
						 final_columns_modify = character(0),
						 final_change_factor = numeric(0)) {
  
  require(reshape2)
  
  # Define all the file paths in global environment
  # paste0() is used in lieu of file.path() to keep the trailing slash
  BEApath <<- paste0(code_path, '/SI/BEA/')
  BLSpath <<- paste0(code_path, '/SI/BLS/')
  Censuspath <<- paste0(code_path, '/SI/Census/')
  Crosswalkpath <<- paste0(code_path, '/SI/Crosswalk/')
  IOMBpath <<- paste0(code_path, '/SI/IOMB/')
  LCIApath <<- paste0(code_path, '/SI/USEEIO/')
  ModelBuildspath <<- paste0(code_path, '/useeiopy/Model Builds/')
  MSWpath <<- paste0(code_path, '/SI/MSW/')
  USEEIOIOpath <<- paste0(code_path, '/SI/USEEIO/IO/')
  USEEIOpath <<- paste0(code_path, '/SI/USEEIO/')
  
  if (!dir.exists(outputfolder)) dir.create(outputfolder)

  #Generate Demand
  source(file.path(code_path, 'R/Demand/Demand_fixed.R'))
  source(file.path(code_path, 'R/Demand/USDemandFunctions_fixed.R'))
  #StartYear,EndYear,CPIRefYear
  
  ### Added 29 May 2019: Here, include modifications to the final demand vector for the scenario.
  USEEIO_Demand = getUSTotalConsProd(Y,Y,2013,usetablefile=usetablefile, columns_modify = final_columns_modify, rows_modify = final_rows_modify, row_factor = final_change_factor)
  
  #Generate IO tables 
  source(file.path(code_path, "R/IO/IOfunctions_fixed.r"))
  US_marketshares = generateMarketShareCoefficientfromMake(maketablefile, year=Y)
  USEEIO_MarketShares =  formatIOTableforIOMB(US_marketshares, majormodelversion = majormodelversion)
  CommodityByCommodityDirectRequirementsCoefficients = generateDR1RegionCoeffs(year = Y, usetablefile = usetablefile, maketablefile = maketablefile)
  USEEIO_DRC = formatIOTableforIOMB(CommodityByCommodityDirectRequirementsCoefficients)
  
  ### Added 29 May 2019: Here, include modifications to the DRC table for the scenario.
  if (length(intermediate_columns_modify) > 0) {
	USEEIO_DRC[, intermediate_columns_modify] <- sweep(USEEIO_DRC[, intermediate_columns_modify, drop = FALSE], 2, intermediate_change_factor, '*')
  }
  
  #Generate the LCIA factor table
  # This part is unchanged from 2007 to 2012.
  source(file.path(code_path, 'R/General Functions/LCIAFunctions.R'))
  USEEIO_LCIA = generateLCIA(modelversion)
  
  #Write all files to the apprpriate directory for the model build
  write.csv(USEEIO_Demand,paste(outputfolder,'/',model,"_FinalDemand.csv",sep=""),row.names = FALSE)
  write.csv(USEEIO_MarketShares,paste(outputfolder,'/',model,'_MarketShares.csv',sep=""),row.names = TRUE)
  write.csv(USEEIO_DRC,paste(outputfolder,'/',model,"_DRC.csv",sep=""),row.names = TRUE)
  write.csv(USEEIO_LCIA,paste(outputfolder,'/',model,"_LCIA.csv",sep=""),row.names = FALSE)
  
  # Copy the other files to the directory.
  file_names <- c('USEEIO_compartment_meta_data.csv', 'USEEIOv1.1_sat_c.csv', 'USEEIOv1.1_satellitefiles.csv', 'USEEIOv1.1_sector_meta_data.csv')
  new_file_names <- c('USEEIO_compartment_meta_data.csv', paste(model, c('sat_c.csv', 'satellitefiles.csv', 'sector_meta_data.csv'), sep = '_'))
  
  for (i in 1:length(file_names)) file.copy(from = file.path(paste(ModelBuildspath,"USEEIOv1.1",sep=""), file_names[i]),
                                            to = file.path(outputfolder, new_file_names[i]))
  
}