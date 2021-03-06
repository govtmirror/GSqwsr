#'Create log and interaction columns
#'
#'Create log and interaction columns. Using a formula and raw data, log and interaction columns will be computed.
#'If negative values are found in the raw data column that need to be log transformed, the values are changed to 1. 
#'
#'@param formulaToUse formula
#'@param localDT dataframe of potential input variables to model
#'@return fullDTList list of DT (data frame with new log and interaction columns), 
#'modelFormula (text of formula to use with new column names), 
#'and colNames (all column names)
#'@keywords transforms
#'@export
#'@examples
#' DTComplete <- StLouisDT
#' colnames(DTComplete) <- gsub("_Inst","",colnames(DTComplete)) 
#' UV <- StLouisUV
#' colnames(UV) <- gsub("_Inst","",colnames(UV)) 
#' response <- "Ammonia.N"
#' DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
#' DT <- na.omit(DT)
#' basicFormula <- "Flow + log(Flow) + Wtemp:Flow"
#' returnData <- createFullDT(basicFormula,DT)
#' newDT <- returnData$DT
#' modelFormula <- returnData$modelFormula
#' newColumns <- returnData$colNames
createFullDT <- function(formulaToUse, localDT){
  
  splt <- unlist(strsplit(formulaToUse, " \\+ "))
  
  interactionIndex <- grep("\\:",splt )
  interactionVariables <- c()
  
  if(length(interactionIndex) != 0){
    interactionNames <- splt[interactionIndex]
    
    #Set up new interaction columns:
    for (i in interactionIndex){
      vars <- strsplit(splt[i],"\\:")[[1]]
      
      logVars <- grep("log\\(",vars )
      
      if(length(logVars)>0){
        if (length(logVars)>1){
          rawVars <- substr(vars,5,nchar(vars)-1)          
          colName <- paste("log",rawVars[1],"Andlog",rawVars[2],sep="")
          localDT[[colName]]<- log(localDT[[rawVars[1]]])*log(localDT[[rawVars[2]]])
          
        } else {
          logSingleName <- vars[logVars]
          logSingleName <- substr(logSingleName,5,nchar(logSingleName)-1)
          nonLogName <- vars[-logVars]
          colName <- paste(nonLogName,"Andlog",logSingleName,sep="")
          localDT[[colName]] <- localDT[[nonLogName]]*log(localDT[[logSingleName]])          
        }
        interactionVariables <- c(interactionVariables,colName)
      } else {
        colName <- paste(vars,collapse="And")
        localDT[[colName]] <- localDT[[vars[1]]]*localDT[[vars[2]]]
        
        interactionVariables <- c(interactionVariables,colName)
      }

      splt[i] <- colName
    }    
    
    
    spltReduced <- splt[-interactionIndex]
  } else {
    interactionVariables <- NA
    spltReduced <- splt
  }

  logIndex <- grep("log\\(",spltReduced )
  
  if (length(logIndex)>0){
    logVariables <- substr(spltReduced[logIndex],5,nchar(spltReduced[logIndex])-1)
    colName <- paste("log",logVariables,sep="")
    
    for(k in logVariables){
      localDT[localDT[k] <= 0 & !is.na(localDT[k]),k] <- 1
    }
    
    localDT[colName] <- log(localDT[logVariables])
    
    splt[logIndex] <- colName
  }
  
  modelFormula <- paste(splt,collapse=" + ")
  
  fullDTList <- list(DT=localDT, modelFormula=modelFormula, colNames=splt)
  
  return(fullDTList)

}