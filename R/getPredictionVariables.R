#' Returns a character vector of prediction variables.
#' 
#' Returns a character vector of prediction variables. Accepts column names as an input, this function 
#' filters out '_cd' columns as well as "agency", "site", "tz". These are typical column names
#' retrieved from NWIS web services.
#'
#'@param DTnames column names of DT dataframe
#'@return predictVariables string predict variables based on column headers
#'@keywords predict
#'@export
#'@examples
#' UV <- StLouisUV
#' colnames(UV) <- gsub("_Inst","",colnames(UV)) 
#' predictVars <- getPredictVariables(names(UV))
getPredictVariables <- function(DTnames){
  splitNames <- sapply(strsplit(DTnames, "_"),function(x)x[length(x)])
  splitNamesAvoid <- sapply(strsplit(DTnames, "_"),function(x)x[1])
  
  valueIndex <- which("cd" != splitNames & !(splitNamesAvoid %in% c("agency", "site", "tz","DATE","datetime")))
  
  predictVariables <- DTnames[valueIndex]
  return(predictVariables)
}