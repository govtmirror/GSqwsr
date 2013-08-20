#'findOutliers
#'
#'Find index of outliers using external studentized residuals.
#'
#'@param localDT DTframe that includes all response and predictor variables
#'@param modelReturn list returned from censReg
#'@param transformResponse string can be "normal" or "lognormal", perhaps try to generalize this more in future
#'@return outlier vector of index numbers
#'@keywords studentized residuals
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
#' outlierIndex <- findOutliers(modelReturn,DT)
findOutliers <- function(modelReturn, localDT, transformResponse="lognormal"){
  modelCoef <- modelReturn$PARAML
  names(modelCoef) <- c(dimnames(modelReturn$XLCAL)[[2]],"logSigma")
  modelCoefList <- list()

  modelCoefList[[rownames(attributes(modelReturn$terms)$factors)[1]]] <- modelCoef
  
  StRes.all.extReturn <- externalStudentRes(localDT, modelCoefList,transformResponse)
  outlier <- which(StRes.all.extReturn$StRes.all.ext > 3 | StRes.all.extReturn$StRes.all.ext < -3)
  return(outlier)
  
}