#'analyzeSteps
#'
#'Plot showing improvement in model choices
#'
#'@param steps dataframe
#'@param responseVariable string column header of single response variable to model
#'@param xCorner number represents the x-upper-left corner of legend in \% plot area
#'@param yCorner number represents the y-upper-left corner of legend in \% plot area
#'@param siteINFO dataframe including station name (station.nm) and siteID (site.no) (easiestly retrieved from dataRetrieval package)
#'@return plot
#'@keywords scatterplot
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
#' steps <- returnPrelim$steps
#' analyzeSteps(steps,response,siteINFO)
analyzeSteps <- function(steps, responseVariable, siteINFO, xCorner = 0.6, yCorner=0.35){
  
  parOriginal <- par(no.readonly = TRUE)
  
  par(mar=c(5,12,5,5))
  
  with(steps, plot(Correlation, xlab="Step", ylab="Corrolation, Slope, RMSE", 
                   ylim=c(0,1), axes=FALSE, pch=20, col="black",type="o"))
  with(steps, points(Slope, pch=20, col="red",type="o"))
  with(steps, points(Res.St.Error, pch=20, col="blue",type="o"))
  axis(2,  ylim=c(0,1))
  axis(1,  xlim=c(1,length(steps)))
  par(new=TRUE)
  with(steps, plot(PRESS, axes=FALSE, xlab="", ylab= "",
                   pch=20, col="forestgreen", ylim=c(0,max(PRESS,na.rm=TRUE)),type="o"))
  axis(4,  ylim=c(0,max(steps$PRESS,na.rm=TRUE)))
  mtext("PRESS", side=4, line=2)
  par(new=TRUE)
  with(steps, plot(AIC, axes=FALSE, xlab="", ylab= "",
                   pch=20, col="grey", ylim=c(min(AIC,na.rm=TRUE),max(AIC,na.rm=TRUE)),type="o"))
  axis(2,  ylim=c(min(steps$AIC,na.rm=TRUE),max(steps$AIC,na.rm=TRUE)),line=5)
  mtext("'AIC'", side=2, line=7)
  box()
  x1 <- grconvertX(xCorner, from="npc", to="user")
  y1 <- grconvertY(yCorner, from="npc", to="user")  
  legend(x1, y1, c("Correlation", "Slope", "RMSE", "PRESS","'AIC'"), 
         pch=c(20, 20, 20, 20,20),col=c("black", "red","blue","forestgreen","grey"),bg="white")
  
  prettyName <- simpleCap(siteINFO$station.nm)
  prettyName <- gsub("Wi", "WI",prettyName) #Consider other states.
  
  title(paste(prettyName, " \n ", responseVariable, sep=""))
  
  par(parOriginal)

}