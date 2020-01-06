# Test modification with altered impacts.

#Generate components of USEEIOv1.1 model
#currently produces:
# Final demand - total consumption and production vectors for 2007
# Direct requirements coefficients
# Market shares
# LCIA factors
#See the README in the model folder for info on preparation of other model components

#Set output to where you want 
#must create output folder in advance
outputfolder = paste(ModelBuildspath,"USEEIO_testmodification/",sep="")
model = "USEEIO_testmodification"
modelversion = "v1.1"
majormodelversion=1

dir.create(outputfolder)

#Generate Demands
source('R/Demand/Demand.R')
#StartYear,EndYear,CPIRefYear
USEEIO_Demand = getUSTotalConsProd(2007,2007,2013)

#Generate IO tables 
source("R/IO/IOFunctions.R")
US_marketshares = generateMarketShareCoefficientfromMake2007()
USEEIO_MarketShares =  formatIOTableforIOMB(US_marketshares)
CommodityByCommodityDirectRequirementsCoefficients = generateDR1RegionCoeffs()
USEEIO_DRC = formatIOTableforIOMB(CommodityByCommodityDirectRequirementsCoefficients)

# Manipulate the variable such that food stores require more input from canning sector.
freshveg_idx <- grep('fresh vegetables', dimnames(USEEIO_DRC)[[1]])
packaging_idx <- grep('packag', dimnames(USEEIO_DRC)[[1]], value = TRUE)
cans_idx <- grep('cans', dimnames(USEEIO_DRC)[[1]], value = TRUE)
foodstore_idx <- grep('(?=.*food)(?=.*store)', dimnames(USEEIO_DRC)[[1]], value = TRUE, perl = TRUE)

USEEIO_DRC[cans_idx, foodstore_idx] <- USEEIO_DRC[cans_idx, foodstore_idx] * 2

#Generate the LCIA factor table
source('R/General Functions/LCIAFunctions.R')
USEEIO_LCIA = generateLCIA(modelversion)

#Write all files to the apprpriate directory for the model build
write.csv(USEEIO_Demand,paste(outputfolder,model,"_FinalDemand.csv",sep=""),row.names = FALSE)
write.csv(USEEIO_MarketShares,paste(outputfolder,model,'_MarketShares.csv',sep=""),row.names = TRUE)
write.csv(USEEIO_DRC,paste(outputfolder,model,"_DRC.csv",sep=""),row.names = TRUE)
write.csv(USEEIO_LCIA,paste(outputfolder,model,"_LCIA.csv",sep=""),row.names = FALSE)

