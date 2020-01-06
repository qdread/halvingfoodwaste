#Detail level codes
demandcodes_detail = c("F01000","F02S00","F02E00","F02N00","F02R00","F03000", "F04000",
                       "F05000","F06C00","F06S00","F06E00","F06N00","F07C00","F07S00",
                       "F07E00","F07N00","F10C00","F10S00","F10E00","F10N00")


# Fixed to get use table that I created for any year.
# Modified 29 May 2019: include the modification for scenario demand change in this function.
getUSDetailDemandfromUseTable = function (year, usetablefile, columns_modify = character(0), rows_modify = character(0), R_row = numeric(0)) {
  
  UseDetail = read.table(usetablefile, sep = ",",header=T,row.names=1,check.names=F,nrows=389)
  # Replace NAs with 0
  UseDetail[is.na(UseDetail)]=0
  # Import (F05000) of 2122A0 was 1439, corrected it to -2561,a value from the import matrixes for this cell  
  #UseDetail07["2122A0", "F05000"] = -2561
  #Convert from million to USD
  FinalDemand_Detail = UseDetail[,demandcodes_detail]*1E6
  
  # Here include the demand changes in the scenario.
  if (length(columns_modify) > 0 & length(rows_modify) > 0) {
	FinalDemand_Detail[rows_modify, columns_modify] <- sweep(FinalDemand_Detail[rows_modify, columns_modify, drop = FALSE], 1, R_row, '*')
  }
  return(FinalDemand_Detail)
}

