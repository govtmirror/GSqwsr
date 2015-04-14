#'Prediction plot of model results over time.
#'
#'Using unit value data, returns a plot of the model results. Predictions are bias corrected if 
#'transform response is set to "lognormal". Predictions are plotted as a blue line, actual sample
#'measurements are plotted as solid red points. Censored values are distinguished with line segments,
#'and the points are .
#'The plot title is generated from the siteINFO dataframe, which needs columns station_nm and site.no. 
#'
#'@param localUV dataframe of unit values, which needs to includes one column called "datetime" that is in POSIXct.
#'@param localDT dataframe in wide format, which needs to includes one column called "datetime" that is in POSIXct.
#'@param finalModel censReg model results
#'@param transformResponse string can be "lognormal" or "normal", perhaps try to generalize this more in future
#'@param siteINFO dataframe including station name (station_nm) and siteID (site.no) (easiestly retrieved from dataRetrieval package)
#'@keywords scatterplot
#'@export
#'@examples
#' DTComplete <- StLouisDT
#' colnames(DTComplete) <- gsub("_Inst","",colnames(DTComplete)) 
#' UV <- StLouisUV
#' colnames(UV) <- gsub("_Inst","",colnames(UV)) 
#' response <- "Ammonia.N"
#' siteINFO <- StLouisInfo
#' DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
#' DT <- na.omit(DT)
#' kitchenSink <- createFullFormula(DT,response)
#' returnPrelim <- prelimModelDev(DT,response,kitchenSink)
#' modelReturn <- returnPrelim$DT.mod
#' predictionPlot(UV,DT,modelReturn,siteINFO=siteINFO)
predictionPlot <- function(localUV,localDT,finalModel,transformResponse="lognormal",siteINFO){
  
  parOriginal <- par(no.readonly = TRUE)
  
  responseVariable <- rownames(attributes(finalModel$terms)$factors)[1]

  terms <- attributes(finalModel$terms)$term.labels
  formulaToUse <- paste(terms,collapse=" + ")
  
  newUVList <- createFullDT(formulaToUse, localUV)
  colNames <- c("",newUVList$colNames)
  newUV <- newUVList$DT
  
  if ("sinDY" %in% newUVList$colNames | "cosDY" %in% newUVList$colNames | "decYear" %in% newUVList$colNames){
    decYear <- getDecYear(newUV$datetime)
    newUV$decYear <- decYear
    newUV$sinDY <- sin(decYear * 2 * pi)
    newUV$cosDY <- cos(decYear * 2 * pi)
  }
  newUV <- newUV[,which(names(newUV) %in% c("datetime",newUVList$colNames))]
  newUV <- na.omit(newUV)
  
  if (!is.null(names(newUV))){
    if (ncol(newUV) > 2){
      testFinite <- apply(newUV[,names(newUV) != "datetime"], 1, function(x) all(is.finite(x)))
    } else {
      testFinite <- is.finite(newUV[,names(newUV) != "datetime"])
    }
    newUV <- newUV[testFinite,]
  } else {
    newUV <- data.frame(datetime=newUV)
  }
  row.names(newUV) <- NULL

  predVal <- runPred(newUV,localDT,finalModel,transformResponse,dfReady=TRUE)
  
  logPlot <- ifelse("lognormal" == transformResponse, "y", "")
  
  if(sum(predVal) == 0){
    cat("All predictions came back as zero:", responseVariable,"\n")
    plot(newUV$datetime,predVal,
         xlab="Date", ylab=responseVariable,col="blue",type="l")    
  } else {
    plot(newUV$datetime,predVal,
         xlab="Date", ylab=responseVariable,col="blue",type="l",log=logPlot)  
  }
  
  points(localDT$datetime, localDT[[responseVariable]]@.Data[,2],col="red",pch=20)
  if (sum(finalModel$CENSFLAG) > 0){
    cenValsY <- localDT[[responseVariable]][finalModel$CENSFLAG]
    cenValsX <- localDT$datetime[finalModel$CENSFLAG]
    
    segments(x0=cenValsX, y0=cenValsY, x1=cenValsX, y1=0.0001,col="red")
  }
  
  prettyName <- simpleCap(siteINFO$station_nm)
  prettyName <- gsub("Wi", "WI",prettyName) #Consider other states.
  
  title(paste(responseVariable, " at ", prettyName, " (", siteINFO$site.no, ")", sep=""))
  
  terms <- attributes(finalModel$terms)$term.labels
  formulaToUse <- paste(terms,collapse=" + ")
  formulaToUse <- paste(responseVariable, " ~ ", formulaToUse)
  mtext(formulaToUse, side=3, line=0.5,cex=0.7)
  
  par(parOriginal)

  DFpredicted <- data.frame(dateTime=newUV$datetime,predVal=predVal)
  
  obs_pred <- mergeNearest(localDT[,c("datetime",responseVariable)], dates.left="datetime", all.left=TRUE,
                     right=DFpredicted, dates.right="dateTime", max.diff="2 hours")


}