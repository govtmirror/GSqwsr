#'createModelEquation
#'
#'Create the model equation including coefficients.
#'
#'@param modelReturn censReg model results
#'@return combo string
#'@keywords equation
#'@export
#'@examples
#' DTComplete <- DTComplete
#' UV <- UV
#' QWcodes <- QWcodes
#' siteINFO <- siteINFO
#' response <- QWcodes$colName[1]
#' DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
#' DT <- na.omit(DT)
#' kitchenSink <- createFullFormula(DT,response)
#' returnPrelim <- prelimModelDev(DT,response,kitchenSink)
#' modelReturn <- returnPrelim$DT.mod
#' createModelEquation(modelReturn)
createModelEquation <- function(modelReturn){
  responseVariable <- rownames(attributes(modelReturn$terms)$factors)[1]
  
  termNames <- names(coef(modelReturn))
  termNames[1] <- ""
  coefficients <- round(as.numeric(coef(modelReturn)),digits=4)
  distribution <- modelReturn$dist
  
  if ("lognormal" == distribution){    
    responseVariable <- paste("ln(",responseVariable,")",sep="")
  }
  
  combo <- paste( round(coef(modelReturn),digits=4), names(coef(modelReturn)))
  
  combo <- paste(combo, collapse=" + ")
  
  combo <- paste(responseVariable, " = ", combo, sep="")
  return(combo)
  
}