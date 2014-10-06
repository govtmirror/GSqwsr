## ----openLibrary, echo=FALSE------------------------------
library(xtable)
options(continue=" ")
options(width=60)
library(knitr)


## ----include=TRUE ,echo=FALSE,eval=TRUE-------------------
opts_chunk$set(highlight=TRUE, tidy=TRUE, keep.space=TRUE, keep.blank.space=FALSE, keep.comment=TRUE, tidy=FALSE,comment="")
knit_hooks$set(inline = function(x) {
   if (is.numeric(x)) round(x, 3)})
knit_hooks$set(crop = hook_pdfcrop)

bold.colHeaders <- function(x) {
  x <- gsub("\\^(\\d)","$\\^\\1$",x)
  x <- gsub("\\%","\\\\%",x)
  x <- gsub("\\_"," ",x)
  returnX <- paste("\\multicolumn{1}{c}{\\textbf{\\textsf{", x, "}}}", sep = "")
}
addSpace <- function(x) ifelse(x != "1", "[5pt]","")

## ----start,eval = FALSE-----------------------------------
#  library("GSqwsr")
#  
#  #Sample data included with package:
#  DTComplete <- StLouisDT
#  UV <- StLouisUV
#  QWcodes <- StLouisQWcodes
#  siteINFO <- StLouisInfo
#  
#  investigateResponse <- "Ammonia.N"
#  transformResponse <- "lognormal"
#  
#  DT <- DTComplete[c(investigateResponse,
#                     getPredictVariables(names(UV)),
#                     "decYear","sinDY","cosDY","datetime")]
#  DT <- na.omit(DT)
#  
#  predictVariables <- names(DT)[-which(names(DT)
#                    %in% c(investigateResponse,"datetime","decYear"))]
#  
#  
#  #Check predictor variables
#  predictVariableScatterPlots(DT,investigateResponse)
#  
#  # Create 'kitchen sink' formula:
#  kitchenSink <- createFullFormula(DT,investigateResponse)
#  
#  #Run stepwise regression with "kitchen sink" as upper bound:
#  returnPrelim <- prelimModelDev(DT,investigateResponse,kitchenSink,
#                                 "BIC", #Other option is "AIC"
#                                 transformResponse)
#  
#  steps <- returnPrelim$steps
#  modelResult <- returnPrelim$modelInformation
#  modelReturn <- returnPrelim$DT.mod
#  
#  # Analyze steps found:
#  plotSteps(steps,DT,transformResponse)
#  analyzeSteps(steps, investigateResponse,siteINFO)
#  
#  # Analyze model produced from stepwise regression:
#  resultPlots(DT,modelReturn,siteINFO)
#  resultResidPlots(DT,modelReturn,siteINFO)
#  
#  # Create prediction plots
#  predictionPlot(UV,DT,modelReturn,siteINFO=siteINFO)
#  

## ----openGSqwsr,echo=TRUE,eval=FALSE----------------------
#  library(GSqwsr)

## ----openGSqwsrHidden,echo=FALSE,eval=TRUE, message=FALSE----
library(GSqwsr)

## ----whatQW,echo=TRUE,eval=TRUE---------------------------

site <- "04024000"  
QWcodes <- whatQW(site, minCount=20)
head(QWcodes)

## ----importNWISqw,echo=TRUE,eval=FALSE--------------------
#  pCodeQW <- c("00608","00613","00618")
#  startDate <- "2011-04-22"
#  endDate <- ""
#  QW <- importNWISqw(site, params=pCodeQW,
#                     begin.date=startDate, end.date=endDate)

## ----importNWISqwHidden,echo=FALSE,eval=TRUE--------------
pCodeQW <- c("00608","00613","00618")
startDate <- "2011-04-22"
endDate <- ""
QW <- StLouisQW

## ----qwColNames,echo=TRUE,eval=TRUE-----------------------
names(QW)

## ----makeQWObjects,echo=TRUE,eval=FALSE-------------------
#  
#  QWRaw <- retrieveNWISqwData(site,pCodeQW,startDate,
#                              endDate,expanded=TRUE)
#  QW <- makeQWObjects(QWRaw)

## ----getDataAvailability,echo=TRUE,eval=TRUE, message=FALSE----
library(dataRetrieval)
UVcodes <- getNWISDataAvailability(site)
UVcodes <- UVcodes[UVcodes$service == "uv",]
names(UVcodes)
UVcodes$parameter_cd

## ----getMultipleUV,echo=TRUE,eval=FALSE-------------------
#  UVpCodes <- c("00010","00060","00095","00300","00400","63680")
#  UV <- getMultipleUV(site, startDate, endDate, UVpCodes)
#  

## ----getMultipleUVHidden,echo=FALSE,eval=TRUE-------------
UVpCodes <- c("00010","00060","00095","00300","00400","63680")
UV <- StLouisUV

## ----uvColNames,echo=TRUE,eval=TRUE-----------------------
names(UV)

## ----mergeDatasets,echo=TRUE,eval=TRUE--------------------
QW$datetime <- as.POSIXct(paste(QW$sample_dt," ",QW$sample_tm, ":00",sep=""))

# Make sure they are in consistant time zones:
QW$datetime <- setTZ(QW$datetime, QW$tzone_cd)
UV$datetime <- setTZ(UV$datetime, UV$tz_cd)

mergeReturn <- mergeDatasets(QW, UV, QWcodes)
DTComplete <- mergeReturn$DTComplete
QWcodes <- mergeReturn$QWcodes

## ----getDT,echo=TRUE,eval=TRUE----------------------------
investigateResponse <- "Nitrate.N"
predictionVariables <- getPredictVariables(names(UV))

DT <- DTComplete[c(investigateResponse,
                   predictionVariables, 
                   "decYear","sinDY","cosDY","datetime")]

names(DT)

## ----rmNADT,echo=TRUE,eval=TRUE---------------------------
DT <- na.omit(DT)

## ----plotQQTransforms,echo=TRUE,eval=TRUE,fig.cap="plotQQTransforms"----
plotQQTransforms(DT,investigateResponse)

## ----predictVariableScatterPlots,echo=TRUE,eval=TRUE,fig.cap="predictVariableScatterPlots", message=FALSE----
predictVariableScatterPlots(DT,investigateResponse)

## ----createFullFormula,echo=TRUE,eval=TRUE,echo=TRUE------
upperBoundFormula <- createFullFormula(DT,investigateResponse)

## ----createFullFormula2,echo=TRUE,eval=TRUE,echo=FALSE,results='markup'----
substring(upperBoundFormula, first=0,last=59)
substring(upperBoundFormula, first=60,last=119)

## ----prelimModelDev,echo=TRUE,eval=TRUE,echo=TRUE---------
transformResponse <- "lognormal"

returnPrelim <- prelimModelDev(DT,
                 investigateResponse,
                 upperBoundFormula,
                 "BIC", #Other option is "AIC"
                 transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelInformation
modelReturn <- returnPrelim$DT.mod

## ----analyzeSteps,echo=TRUE,eval=TRUE,echo=TRUE,fig.cap="analyzeSteps"----
siteINFO <- getNWISSiteInfo(site)
analyzeSteps(steps, investigateResponse,siteINFO,
             xCorner=0.01,yCorner=0.3)

## ----plotSteps,echo=TRUE,eval=TRUE,fig.cap="plotSteps"----

m <- t(matrix(c(1:6), nrow = 2, ncol = 3))
layout(m)
par(mar=c(2,2,2,2))
plotSteps(steps,DT,transformResponse)


## ----generateParamChoices,echo=TRUE,eval=FALSE------------
#  #Change this to a relavent path:
#  pathToSave <- "C:/RData/"
#  choices <- generateParamChoices(predictionVariables,
#                                  modelReturn,pathToSave,save=TRUE)
#  

## ----generateParamChoices2,echo=FALSE,eval=TRUE-----------
choices <- generateParamChoices(predictionVariables,modelReturn)

## ----createFormulaFromDF,echo=TRUE,eval=FALSE-------------
#  choicesNew <- read.csv(pathToSave)
#  newFormula <-createFormulaFromDF(choicesNew)

## ----createFormulaFromDF2,echo=FALSE,eval=TRUE------------
choicesNew <- choices
choicesNew[7,8] <- 1
choicesNew[2,2] <- 1
newFormula <-createFormulaFromDF(choicesNew)

## ----createFormulaFromDF3,echo=TRUE,eval=TRUE-------------
newFormula

## ----censReg, echo=TRUE,eval=TRUE-------------------------
newUpperFormula <- paste(investigateResponse," ~ ", newFormula, sep="")
modelReturnCustom <- censReg(newUpperFormula, 
                       dist=transformResponse, data=DT)
modelReturnCustom

## ----resultPlots,echo=TRUE,eval=TRUE,echo=TRUE,fig.cap="resultPlots"----
resultPlots(DT,modelReturn,siteINFO)

## ----resultResidPlots,echo=TRUE,eval=TRUE,echo=TRUE,fig.cap="resultResidPlots"----
resultResidPlots(DT,modelReturn,siteINFO)

## ----predictionPlot,echo=TRUE,eval=TRUE,echo=TRUE,fig.cap="predictionPlot"----
predictionPlot(UV,DT,modelReturn,siteINFO=siteINFO)

## ----summaryPrintout,echo=TRUE,eval=TRUE,echo=TRUE--------
summaryPrintout(modelReturn, siteINFO)

## ----helpFunc,eval = FALSE--------------------------------
#  library(GSqwsr)
#  ?plotSteps

## ----rawFunc,eval = FALSE---------------------------------
#  plotSteps

## ----installFromCran,eval = FALSE-------------------------
#  
#  install.packages(c("USGSwsBase","USGSwsStats",
#                     "USGSwsData","USGSwsGraphs",
#                     "USGSwsQW","dataRetrieval","GSqwsr"),
#        repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"),
#        dependencies=TRUE)

## ----openLibraryTest, eval=FALSE--------------------------
#  library(GSqwsr)

