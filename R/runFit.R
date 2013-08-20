#'runFit
#'
#'Run the fitting routine
#'
#'@param finalModel results from censReg
#'@param localDT dataframe of potential input variables to model
#'@param transformResponse string can be "normal" or "lognormal", perhaps try to generalize this more in future
#'@return evaluat list
#'@keywords AMLE fit
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
#' runFit(DT,modelReturn)
runFit <- function(localDT,finalModel,transformResponse="lognormal"){
  
  distribution <- transformResponse
  
  investigateResponse <- rownames(attributes(finalModel$terms)$factors)[1]
  
  estimateData <- as.lcens(localDT[[investigateResponse]])

  terms <- attributes(finalModel$terms)$term.labels
  formulaToUse <- paste(terms,collapse=" + ")
  fullDTList <- createFullDT(formulaToUse, localDT)
  localDT <- fullDTList$DT
  newTerms <- fullDTList$colNames
  
  if (sum(names(localDT) %in% newTerms) > 1){
    calibrationData <- as.matrix(cbind(1,localDT[,which(names(localDT) %in% newTerms)]))
    rownames(calibrationData) <- NULL
    colnames(calibrationData)[1] <- ""
  } else {
    calibrationData <- as.matrix(cbind(1,localDT[,which(names(localDT) %in% newTerms)]))
    rownames(calibrationData) <- NULL
    colnames(calibrationData) <- c("",newTerms[1])
  }
  
  evaluat <- censReg_AMLE.fit(estimateData, calibrationData, distribution)
  return(evaluat)
}
