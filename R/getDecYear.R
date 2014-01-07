#'Returns decimal year from POSIXct
#'
#'Returns decimal year from POSIXct.
#'
#'@param dateTime POSIXct
#'@return decYear numeric
#'@keywords dateTime conversion
#'@export
#'@examples
#'date1 <- as.POSIXct("2012-01-02 12:38:00")
#'decYear <- getDecYear(date1)
#'date2 <- as.POSIXct("2012-07-02 12:38:00")
#'decYear2 <- getDecYear(date2)
getDecYear <- function(dateTime){
  dateTime <- as.POSIXlt(dateTime)
#   Julian <- as.numeric(julian(dateTime,origin=as.Date("1850-01-01")))
#   Month <- dateTime$mon + 1
#   Day <- dateTime$yday + 1
  year <- dateTime$year + 1900
#   hour <- dateTime$hour
#   minute <- dateTime$min
#   decYear <- year + (Day -0.5)/366 + (hour)/(24*366) + minute/(24*60*366)
  jan1 <- as.POSIXlt(paste(year,"-01-01",sep=""),format="%Y-%m-%d")
  jan1NextYear <- as.POSIXlt(paste(year+1,"-01-01",sep=""),format="%Y-%m-%d")
  decimal <- as.numeric(difftime(dateTime, jan1, units = "secs"))
  decimal <- decimal/as.numeric(difftime(jan1NextYear, jan1, units = "secs"))
  
  decYear <- year + decimal
  return(decYear)
}