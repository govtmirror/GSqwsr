\name{getMultipleUV}
\alias{getMultipleUV}
\title{getMultipleUV}
\usage{
getMultipleUV(Site, BeginDate, EndDate, UVP)
}
\arguments{
  \item{Site}{character, USGS site ID}

  \item{BeginDate}{string}

  \item{EndDate}{string}

  \item{UVP}{character vector of 5 digit USGS parameter
  codes}
}
\value{
UV dataframe
}
\description{
Get unit value data from multiple parameters.
}
\examples{
site <- "040851385"
startDate <- "2013-06-01"
endDate <- "2013-06-08"
UVP <- c("00060","00095")
UV <- getMultipleUV(site, startDate,endDate,UVP)
}
\keyword{NWIS}
\keyword{data}
\keyword{retrieval}

