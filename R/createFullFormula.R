#'Creates text for the upper bound formula.
#'
#'Creates text for the upper bound formula. Accepts a dataframe DT and the name of the column with
#'the response variable. From that, all other columns except datetime and decYear (decimal year) are
#'used to create an upper bound formula for the stepwise regression. This includes all log transformations
#'of columns that have no values less than or equal to zero. Log transforms are also not allowed on pH data.
#'
#'@param localDT dataframe of potential input variables to model
#'@param responseVariable string column header of single response variable to model
#'@return upperBoundFormula text string of formula that includes all variables and log variables
#'@keywords formula
#'@export
#'@examples
#' DTComplete <- StLouisDT
#' colnames(DTComplete) <- gsub("_Inst","",colnames(DTComplete)) 
#' UV <- StLouisUV
#' colnames(UV) <- gsub("_Inst","",colnames(UV)) 
#' response <- "Ammonia.N"
#' DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
#' DT <- na.omit(DT)
#' upperBoundFormula <- createFullFormula(DT,response)
createFullFormula <- function(localDT,responseVariable){

  predictVariables <- names(localDT)[-which(names(localDT) %in% responseVariable)]
  predictVariables <- predictVariables[which(predictVariables != "datetime")]
  predictVariables <- predictVariables[which(predictVariables != "decYear")]
  
  logVariables <- predictVariables[("pH" != predictVariables) & ("decYear" != predictVariables)]
  # Don't want negative logs:
  logVariables <- names(which(sapply(localDT[,which(names(localDT) %in% logVariables)], function(x) min(as.numeric(x),na.rm=TRUE) ) > 0))
  predictString <- paste(predictVariables,collapse=" + ")
  
  if (length(logVariables) == 0){
    upperBoundFormula <- predictString
  } else {
    logString <- as.character(sapply(paste("log(",logVariables,")",sep=""),function(x)x))
    
    
    logString <- paste(logString,collapse=" + ")
    
    upperBoundFormula <- paste(predictString,logString,sep=" + ")
  }
  return(upperBoundFormula)
}