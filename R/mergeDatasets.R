#'Creates a dataframe of merged datasets.
#'
#'Creates a list of merged datasets, along with updating the QWcodes dataframe. The function looks for
#'qw objects in the QW dataframe, cross-references them with the QWcodes table, and merges those
#'columns with the nearest UV (unit value) data. The function will merge the UV data that is nearest
#'to the QW data, see \link{mergeNearest} for more options on specifying the acceptable time gap between
#'the two datasets. 
#'
#'@param localQW dataframe water quality dataset. Should have qw objects, 
#'along with a column called "datetime" in POSIXct.
#'@param localUV dataframe unit value dataset. Should have one column called "datetime" in POSIXct.
#'@param QWcodes dataframe
#'@param max.diff string default is "2 hours". See \link{mergeNearest} for more options.
#'@return retList list including the DT list and updated QWcodes
#'@keywords merge datasets
#'@export
#'@examples
#' UV <- StLouisUV
#' QW <- StLouisQW
#' QWcodes <- StLouisQWcodes
#' mergeReturn <- mergeDatasets(QW, UV, QWcodes)
#' DTComplete <- mergeReturn$DTComplete
#' QWcodes <- mergeReturn$QWcodes
mergeDatasets <- function(localQW, localUV, QWcodes, max.diff="2 hours"){

  namesToCheck <- getPredictVariables(names(localUV))

  ColName <- ""
  index <- as.numeric(which(sapply(localQW, function(x) class(x)) == "qw"))
  
  for (i in 1:nrow(QWcodes)){
    for(j in names(localQW)[index]){ 
      #     analyte.name <- unique(localQW[,j]@analyte.name[!is.na(localQW[,j]@analyte.name)])
      pcode <- unique(localQW[,j]@unique.code[!is.na(localQW[,j]@unique.code)])
      if (pcode == QWcodes$parameter_cd[i]){
        ColName <- j
        break
      }
    }
    QWcodes$colName[i] <- ColName
  }
    
  for (j in namesToCheck){
    subUV <- localUV[c("datetime", j)]
    subUV <- na.omit(subUV)
    DT <- mergeNearest(localQW["datetime"], dates.left="datetime", all.left=TRUE,
                              right=subUV, dates.right="datetime", max.diff=max.diff)
    DT$datetime.right <- NULL
    if (namesToCheck[1] == j){
      DTmerged <- DT
    } else {
      DTmerged <- merge(DTmerged,DT,by=("datetime.left"))
    }
  }
  
  colnames(DTmerged) <- c("datetime",colnames(DTmerged)[-1])
  
  DT_single <- merge(DTmerged,localQW,by=("datetime"))
  
  DT_single$decYear <- getDecYear(DT_single$datetime)
  DT_single$sinDY <- sin(DT_single$decYear*2*pi)
  DT_single$cosDY <- cos(DT_single$decYear*2*pi)
  
  retList <- list(DTComplete=DT_single, QWcodes=QWcodes)
  return(retList)

}