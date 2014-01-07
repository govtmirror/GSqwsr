#'Run the model prediction
#'
#'Run the model prediction. Takes unit value data (UV) and model results to calculate the predicted values. If 
#'transformResponse is set to "lognormal", a bias correction is included.
#'
#'@param localUV dataframe of unit values
#'@param localDT dataframe in wide format
#'@param finalModel censReg model results
#'@param transformResponse string can be "normal" or "lognormal", perhaps try to generalize this more in future
#'@param dfReady logical if the localDT dataframe already includes time/log columns
#'@return predictedReturn list
#'@keywords AMLE fit
#'@export
#'@examples
#' DTComplete <- StLouisDT
#' response <- "Ammonia.N"
#' UV <- StLouisUV
#' siteINFO <- StLouisInfo
#' DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
#' DT <- na.omit(DT)
#' kitchenSink <- createFullFormula(DT,response)
#' returnPrelim <- prelimModelDev(DT,response,kitchenSink,"BIC","lognormal")
#' modelReturn <- returnPrelim$DT.mod
#' predictedReturn <- runPred(UV, DT, modelReturn,dfReady=FALSE)
runPred <- function(localUV,localDT,finalModel,transformResponse="lognormal",dfReady=TRUE){
  
  evaluat <- runFit(localDT,finalModel,transformResponse)
  orderOfVariables <- colnames(evaluat$XLCAL)

  terms <- attributes(finalModel$terms)$term.labels
  formulaToUse <- paste(terms,collapse=" + ")

  if (!dfReady){
    if ("sinDY" %in% terms | "cosDY" %in% terms | "decYear" %in% terms){
      decYear <- getDecYear(localUV$datetime)
      localUV$decYear <- decYear
      localUV$sinDY <- sin(decYear * 2 * pi)
      localUV$cosDY <- cos(decYear * 2 * pi)
    }
    
    newUVList <- createFullDT(formulaToUse, localUV)
    colNames <- c("",newUVList$colNames)
    newUV <- newUVList$DT
    
    newUV <- newUV[,which(names(newUV) %in% colNames)]
    newUV <- na.omit(newUV)

    if (is.data.frame(newUV)){
      testFinite <- apply(newUV, 1, function(x) all(is.finite(x)))
      newUV <- newUV[testFinite,]
    }
        
  } else {
    newUV <- localUV
  }

  if (length(orderOfVariables[-1]) > 1){
    newUV <- newUV[,orderOfVariables[-1]]
  } else {
    newUV <- newUV[,!(names(newUV) %in% "datetime")]
  }

  PredictionData <- as.matrix(cbind(1, newUV))
  
  rownames(PredictionData) <- NULL
  colnames(PredictionData)[1] <- ""
  
  if(nrow(PredictionData) > 150000){
    index1 <- 1:as.integer(nrow(PredictionData)/2)
    pred1Data <- PredictionData[index1,]
    predictedReturn1 <- censReg_AMLE.pred(evaluat, pred1Data)
    index2 <- c(as.integer((1+nrow(PredictionData)/2)):nrow(PredictionData))
    pred2Data <- PredictionData[index2,]
    predictedReturn2 <- censReg_AMLE.pred(evaluat, pred2Data)
    if("lognormal" == transformResponse){
      predictedReturnData <- c(predictedReturn1$BACKEST, predictedReturn2$BACKEST)
    } else {
      predictedReturnData <- c(predictedReturn1$ESTIM, predictedReturn2$ESTIM)
    }
  } else {
    predictedReturn <- censReg_AMLE.pred(evaluat, PredictionData)
    if("lognormal" == transformResponse){
      predictedReturnData <- predictedReturn$BACKEST
    } else {
      predictedReturnData <- predictedReturn$ESTIM
    }
  }
  
  
  return(predictedReturnData)
}