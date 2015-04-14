#'Create QW dataframe from dataRetrieval output
#'
#'Takes output from expanded \code{retrieveNWISqwData}, and turns the water quality data into qw object columns.
#'
#'@param df dataframe output from \code{retrieveNWISqwData}
#'@return newdf dataframe
#'@keywords NWIS data retrieval
#'@import dataRetrieval
#'@import smwrQW
#'@export
#'@examples
#'site <- "04024000"
#'startDate <- "2010-01-01"
#'endDate <- ""
#'pCodes <- c("00940","00608","00613","00631","62855","00671","00665","80154","00618")
#'library(dataRetrieval)
#'rawQWData <-readNWISqw(site,pCodes,startDate,endDate,expanded=TRUE, reshape=TRUE)
#'QW <- makeQWObjects(rawQWData) 
makeQWObjects <- function(df){
  colNames <- names(df)
  pCodes <- unique(sapply(strsplit(colNames,"_"), function(x) x[length(x)]))
  pCodes <- pCodes[grep("\\d{5}",pCodes)]

  newdf <- df[,(colNames %in% c("startDateTime","site_no","endDateTime"))]
  names(newdf)["startDateTime" == names(newdf)] <- "datetime"
  for(i in pCodes){
    subDF <- df[,grep(i,colNames)]
    shortName <- pcodeColNames()
    shortName <- shortName$col_name[shortName$parm_cd == i]
    
    paramINFO <- readNWISpCode(i)
    
    assign(shortName, as.qw(values=subDF[[grep("result_va_",names(subDF))]], 
                            remark.codes=subDF[[grep("remark_cd_",names(subDF))]],
                            value.codes=subDF[[grep("val_qual_tx_",names(subDF))]],
                            reporting.level=subDF[[grep("rpt_lev_va_",names(subDF))]],
                            reporting.method=subDF[[grep("rpt_lev_cd_",names(subDF))]],
                            reporting.units=rep(paramINFO$parameter_units,nrow(subDF)),
                            analyte.method=subDF[[grep("meth_cd_",names(subDF))]],
                            analyte.name=rep(shortName,nrow(subDF)),
                            unique.code=rep(i,nrow(subDF))))
    newdf[[shortName]] <- get(shortName)
  }
  return(newdf)
}