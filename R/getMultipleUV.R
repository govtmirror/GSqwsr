#'getMultipleUV
#'
#'Get unit value data from multiple parameters.
#'
#'@param Site character, USGS site ID
#'@param BeginDate Date
#'@param UVP character vector of 5 digit USGS parameter codes
#'@return UV dataframe
#'@keywords NWIS data retrieval
#'@export
#'@examples
#'site <- "04027000"
#'start <- Sys.Date()
#'UVP <- c("00060","00010")
#'UV <- getMultipleUV(site, start,UVP)
getMultipleUV <- function(Site, BeginDate, UVP){
  
  message("Getting data: ", UVP[1], "\n")
  UV <- readNWIS(Site, dtype="uv", begin.date=BeginDate, param=UVP[1])
  UV <- renCol(UV) 
  
  if (length(UVP) > 1){
    for (i in UVP[-1]){
      message("Getting data: ", i, "\n")
      possibleError <- tryCatch(
        UVsingle <- readNWIS(Site, dtype="uv", begin.date=BeginDate, param=i),
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
  UVsub[is.na(UVsub)]<- ""
  
  UV[,which("cd" == sapply(strsplit(names(UV), "_"),function(x)x[length(x)]))] <- UVsub
  
#   if ("Flow" %in% names(UV)){  #need to get the right column name, now that renCol is not called
#     UV <- transform(UV, Flow=fillMissing(Flow, span=10))
#   }
  
  return(UV)
}