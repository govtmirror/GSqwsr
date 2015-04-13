#'Get unit value data from multiple parameters.
#'
#'Get unit value data from multiple parameters. A single USGS site id, start and end date
#'(in "YYYY-mm-dd" format), and multiple 5-digit parameter codes are inputs. A single data frame with
#'with all the raw data from the multiple web service calls is returned. 
#'
#'@param Site character, USGS site ID
#'@param BeginDate string
#'@param EndDate string
#'@param UVP character vector of 5 digit USGS parameter codes
#'@return UV dataframe
#'@import dataRetrieval
#'@keywords NWIS data retrieval
#'@export
#'@examples
#'site <- "040851385"
#'startDate <- "2013-06-01"
#'endDate <- "2013-06-08"
#'UVP <- c("00060","00095")
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
  
  UV <- readNWISuv(Site,UVP[1], BeginDate, EndDate)
  UV <- renameNWISColumns(UV) 
  
  if (length(UVP) > 1){
    for (i in UVP[-1]){
      message("Getting data: ", i, "\n")
      possibleError <- tryCatch(
        UVsingle <- readNWISuv(Site, i, BeginDate, EndDate),
        error=function(e) e
      )
      
      if(!inherits(possibleError, "error")){
        #REAL WORK
        UVsingle <- renameNWISColumns(UVsingle)
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