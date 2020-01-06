# Script to use the manually edited use tables I created to separately build a different model for each scenario
# QDR / FWE / 06 Dec 2018

source('R/Model Build Scripts/USEEIO2012_buildfunction.R')

build_USEEIO(outputfolder = 'useeiopy/Model Builds/USEEIO2012_scen0/',
             model = 'USEEIO2012_scen0',
             usetablefile = 'Q:/BEA/scenario_usetables/use2012_scen0.csv')

build_USEEIO(outputfolder = 'useeiopy/Model Builds/USEEIO2012_scen1/',
             model = 'USEEIO2012_scen1',
             usetablefile = 'Q:/BEA/scenario_usetables/use2012_scen1.csv')

build_USEEIO(outputfolder = 'useeiopy/Model Builds/USEEIO2012_scen2/',
             model = 'USEEIO2012_scen2',
             usetablefile = 'Q:/BEA/scenario_usetables/use2012_scen2.csv')

build_USEEIO(outputfolder = 'useeiopy/Model Builds/USEEIO2012_scen3/',
             model = 'USEEIO2012_scen3',
             usetablefile = 'Q:/BEA/scenario_usetables/use2012_scen3.csv')
