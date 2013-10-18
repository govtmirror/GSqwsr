#'getMultipleUV
#'
#'Get unit value data from multiple parameters.
#'
#'@param Site character, USGS site ID
#'@param BeginDate string
#'@param EndDate string
#'@param UVP character vector of 5 digit USGS parameter codes
#'@return UV dataframe
#'@keywords NWIS data retrieval
#'@export
#'@examples
#'site <- "04027000"
#'startDate <- "2012-06-01"
#'endDate <- ""
#'UVP <- c("00060","00010")
#'UV <- getMultipleUV(site, startDate,endDate,UVP)
getMultipleUV <- function(Site, BeginDate, EndDate,UVP){
  
  message("Getting data: ", UVP[1], "\n")
  
#   UV <- retrieveUnitNWISData(Site, UVP[1], BeginDate, EndDate)
#   UV <- renameColumns(UV)
#   
#   if (length(UVP) > 1){
#     for (i in UVP[-1]){
#       message("Getting data: ", i, "\n")
#       possibleError <- tryCatch(
#         UVsingle <- retrieveUnitNWISData(Site, i, BeginDate, EndDate),
#         error=function(e) e
#       )
#       
#       if(!inherits(possibleError, "error")){
#         #REAL WORK
#         UVsingle <- renameColumns(UVsingle)
#         UV <- merge(UV,UVsingle, all=TRUE)
#       } else {
#         message("No data for", i)
#       }
#       
#     }
#   }  
  
  UV <- readNWIS(Site, dtype="uv", begin.date=BeginDate, end.date=EndDate,param=UVP[1])
  UV <- renCol(UV) 
  
  if (length(UVP) > 1){
    for (i in UVP[-1]){
      message("Getting data: ", i, "\n")
      possibleError <- tryCatch(
        UVsingle <- readNWIS(Site, dtype="uv", begin.date=BeginDate, end.date=EndDate, param=i),
        error=function(e) e
      )
      
      if(!inherits(possibleError, "error")){
        #REAL WORK
        UVsingle <- renCol(UVsingle)
        UV <- merge(UV,UVsingle, all=TRUE)
      } else {
        message("No data for", i)
      }

    }
  }
  
  UVsub <- UV[,which("cd" == sapply(strsplit(names(UV), "_"),function(x)x[length(x)]))]
  UVsub <- data.frame(lapply(UVsub, as.character), stringsAsFactors=FALSE)
  UVsub[is.na(UVsub)] <- ""
  
  UV[,which("cd" == sapply(strsplit(names(UV), "_"),function(x)x[length(x)]))] <- UVsub
  
  names(UV)[which("dateTime" == names(UV))] <- "datetime"
  
  return(UV)
}