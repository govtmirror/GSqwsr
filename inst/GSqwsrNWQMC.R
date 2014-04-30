library(dataRetrieval)
library(GSqwsr)

# Required Inputs:
site <- "04137500"
siteName <- "Au Sable River"
##########################
siteINFO <-  getSiteFileData(site, interactive=FALSE)
UVcodes <- whatUVNew(site)

# The reported end date in NWIS must be later than the endDate in the function below or it filters it out 
#(so we aren't retrieving data that ended before the real-time sensors went on line):
QWcodes <- whatQW(site, minCount=20, endDate="2012-06-01",ignoreGroups="Information")
#Enter lists of the QW and UV parameters you are interested in
MyQWList <- c("00940","00608","00613","00631","62855","00671","00665","80154","00618")
QWcodes <- QWcodes[which(QWcodes$parameter_cd %in% MyQWList),]
MyUVList <- c("00010", "00060", "00095", "00400", "63680", "00300") 
UVP <- unique(UVcodes$parameter_cd[which(UVcodes$parameter_cd %in% MyUVList)])
startDate<-strftime(max(UVcodes$startDate[which(UVcodes$parameter_cd %in% UVP)]))
endDate<-""
QWret <- retrieveNWISqwData(site, pCodes=MyQWList, startDate=startDate,endDate=endDate,expanded=TRUE)
QW <- makeQWObjects(QWret)
UV <- getMultipleUV(site, startDate, endDate, UVP)

# Merge UV and QW datasets into one
mergeReturn <- mergeDatasets(QW, UV, QWcodes)
DTComplete <- mergeReturn$DTComplete
QWcodes <- mergeReturn$QWcodes
summary(DTComplete)

pathToSave <- paste("C:/Users/jlthomps/Documents/R/",siteName,sep="")

# Probably want to save data at this point:
save(QW,file=paste(pathToSave,"QW.RData",sep="/"))
save(UV,file=paste(pathToSave,"UV.RData",sep="/"))
save(QWcodes,file=paste(pathToSave,"QWcodes.RData",sep="/"))
save(DTComplete,file=paste(pathToSave,"DTComplete.RData",sep="/"))
save(siteINFO,file=paste(pathToSave,"siteINFO.RData",sep="/"))

##########################
# choose response variable and distribution
transformResponse <- "lognormal" # Options are: "normal" or "lognormal"
investigateResponse <- "Phosphorus_WW.P"
##########################
# Restrict DT to predictor and response variables of interest
DT <- DTComplete[c(investigateResponse,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
# Omit rows with NAs for all variables
DT <- na.omit(DT)
datetime <- DT$datetime

################################################################################
# Play with maximizing rows/columns
subDT <- DTComplete[c(investigateResponse,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
subDT <- subDT[,names(subDT) != investigateResponse]
# 
# #Rows removed to maximize columns (remove rows with any missing values):
DTMaxCols <- na.omit(subDT)
# 
# #Columns removed to maximize rows (Remove columns with more than 10% NAs):
DTMaxRows <- subDT[,colSums(is.na(subDT)) <= nrow(subDT)*0.1]  
DTMaxRows <- na.omit(DTMaxRows)
# 
# #List columns removed to maximize rows:
names(DTMaxCols)[!(names(DTMaxCols) %in% names(DTMaxRows))]
dim(DTMaxCols)
dim(DTMaxRows)
# #Choose which to use: DTMaxCols or DTMaxRows:
DT <- DTComplete[,c(investigateResponse,names(DTMaxCols))]
DT <- na.omit(DTMaxCols)
# 
DT <- DTComplete[,c(investigateResponse,names(DTMaxRows))]
DT <- na.omit(DT)

##########################################################

##########################################################
# Preliminary Assessment Plots:
pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_InitialQQGraphs.pdf",sep=""))
plotQQTransforms(DT,investigateResponse)
predictVariableScatterPlots(DT,investigateResponse)
dev.off()
##########################################################

#################################################################################################
# Create kitchen sink formula:
predictVariables <- names(DT)[-which(names(DT) %in% investigateResponse)]
predictVariables <- predictVariables[which(predictVariables != "datetime")]
predictVariables <- predictVariables[which(predictVariables != "decYear")]

kitchenSink <- createFullFormula(DT,investigateResponse)

returnPrelim <- prelimModelDev(DT,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod

#Save plotSteps to file:
pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotSteps(steps,DT,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

vif(modelReturn)

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
##########################################################

##########################################################
# Import model parameters from csv file:
pathToParam <- paste(pathToSave,"/",investigateResponse,"ModelParams.csv",sep="")
choicesNew <- read.csv(pathToParam)
newFormula <-createFormulaFromDF(choicesNew)
##########################################################

##########################################################
#Example of how to remove auto-generated outliers:
outliers <- findOutliers(modelReturn,DT,transformResponse)
DT[outliers,]
if(length(outliers) >0) DT <- DT[-outliers,]
##########################################################

##########################################################
#If you want to re-do stepwise regression:
returnPrelimNew <- prelimModelDev(DT,investigateResponse,kitchenSink,
                                  transformResponse=transformResponse)
steps <- returnPrelimNew$steps
modelResult <- returnPrelimNew$modelStuff
modelReturn <- returnPrelimNew$DT.mod
plotSteps(steps,DT,transformResponse)
##########################################################


##########################################################
# Or, don't do the stepwise regression, just get the model coefficients using csv file:
modelReturn <- censReg(paste(investigateResponse," ~ ", newFormula, sep=""), dist=transformResponse, data=DT)
#####################################################

#####################################################
#Save NEW plotSteps to file:
pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_plotSteps_2.pdf",sep=""))
plotSteps(steps,DT,transformResponse)
dev.off()
#####################################################
pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_analyzeSteps_2.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

vif(modelReturn)

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(DT,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(DT,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Plot predictions using unit values:
pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_prediction_2.pdf",sep=""))
predictionPlot(UV,DT,modelReturn,transformResponse,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/",investigateResponse,"/", investigateResponse,"Summary_2.txt", sep="")

sink(fileName)
summaryPrintout(modelReturn, siteINFO, saveOutput=FALSE,fileName)
EO_bias <- sum(DT$Flow*modelReturn$YPRED)/sum(DT$Flow*DT[[investigateResponse]]@.Data[,2])
cat("E/O bias: ",EO_bias)
sink()

#####################################################

#Want to save a dataframe (aka, save an output)?
fileToSave <- paste(pathToSave, "modelResult.csv",sep="/")
write.table(modelResult, fileToSave, row.names=FALSE, sep=",")  

#Want to export all the UV data to csv file?
fileToSave <- paste(pathToSave, "UV.csv",sep="/")
write.table(UV, fileToSave, row.names=FALSE, sep=",") 

############################################################################