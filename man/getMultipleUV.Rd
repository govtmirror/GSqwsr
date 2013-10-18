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
site <- "04027000"
startDate <- "2012-06-01"
endDate <- ""
UVP <- c("00060","00010")
UV <- getMultipleUV(site, startDate,endDate,UVP)
}
\keyword{data}
\keyword{NWIS}
\keyword{retrieval}

