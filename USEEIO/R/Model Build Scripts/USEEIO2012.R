#Generate components of USEEIOv1.1 model
#currently produces:
# Final demand - total consumption and production vectors for 2012
# Direct requirements coefficients
# Market shares
# LCIA factors
#See the README in the model folder for info on preparation of other model components

# Version created by QDR 09 Nov 2018: Uses 2012 BEA input-output tables (older version used 2007)

#Set output to where you want 
#must create output folder in advance
outputfolder = paste(ModelBuildspath,"USEEIO2012/",sep="")
dir.create(outputfolder)
model = "USEEIO2012"
modelversion = "v1.1"
majormodelversion=1
BEApath <- 'Q:/BEA/formatted/'

Y <- 2012

#Generate Demand
source('R/Demand/Demand_fixed.R')
#StartYear,EndYear,CPIRefYear
USEEIO_Demand = getUSTotalConsProd(Y,Y,2013)

#Generate IO tables 
source("R/IO/IOFunctions_fixed.r")
US_marketshares = generateMarketShareCoefficientfromMake(year=Y)
USEEIO_MarketShares =  formatIOTableforIOMB(US_marketshares)
CommodityByCommodityDirectRequirementsCoefficients = generateDR1RegionCoeffs(year=Y)
USEEIO_DRC = formatIOTableforIOMB(CommodityByCommodityDirectRequirementsCoefficients)

#Generate the LCIA factor table
# This part is unchanged from 2007 to 2012.
source('R/General Functions/LCIAFunctions.R')
USEEIO_LCIA = generateLCIA(modelversion)

#Write all files to the apprpriate directory for the model build
write.csv(USEEIO_Demand,paste(outputfolder,model,"_FinalDemand.csv",sep=""),row.names = FALSE)
write.csv(USEEIO_MarketShares,paste(outputfolder,model,'_MarketShares.csv',sep=""),row.names = TRUE)
write.csv(USEEIO_DRC,paste(outputfolder,model,"_DRC.csv",sep=""),row.names = TRUE)
write.csv(USEEIO_LCIA,paste(outputfolder,model,"_LCIA.csv",sep=""),row.names = FALSE)

