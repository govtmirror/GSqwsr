#' whatUVNew
#'
#' Imports a table of available parameters, period of record, and count. There is also an option to load the long parameter names and additional information on the parameters with longNames=TRUE.
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param service string defaults to "uv" (unit values), other options are "qw" (water quality) and "dv" (daily values)
#' @keywords data import USGS web service
#' @return retval dataframe with all information found in the expanded site file
#' @export
#' @examples
#'site <- "04027000"
#'uv <-whatUVNew(site)
whatUVNew <- function(siteNumber,service="uv"){
  

  urlSitefile <- paste("http://waterservices.usgs.gov/nwis/site?format=rdb&seriesCatalogOutput=true&sites=",siteNumber,sep = "")
  
  SiteFile <- read.delim(  
    urlSitefile, 
    header = TRUE, 
    quote="\"", 
    dec=".", 
    sep='\t',
    colClasses=c('character'),
    fill = TRUE, 
    comment.char="#")
  
  SiteFile <- SiteFile[-1,]
  
  SiteFile <- with(SiteFile, data.frame(parameter_cd=parm_cd, statCd=stat_cd, startDate=begin_date,endDate=end_date, count=count_nu,service=data_type_cd,stringsAsFactors = FALSE))
  
  SiteFile <- SiteFile[!is.na(SiteFile$parameter_cd),]
  SiteFile <- SiteFile["" != SiteFile$parameter_cd,]
  SiteFile$startDate <- as.Date(SiteFile$startDate)
  SiteFile$endDate <- as.Date(SiteFile$endDate)
  SiteFile$count <- as.numeric(SiteFile$count)
  
  SiteFile <- SiteFile[service == SiteFile$service,]
  
  return(SiteFile)
}