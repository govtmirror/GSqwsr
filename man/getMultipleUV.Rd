\name{getMultipleUV}
\alias{getMultipleUV}
\title{getMultipleUV}
\usage{
  getMultipleUV(Site, BeginDate, UVP)
}
\arguments{
  \item{Site}{character, USGS site ID}

  \item{BeginDate}{Date}

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
start <- Sys.Date()
UVP <- c("00060","00010")
UV <- getMultipleUV(site, start,UVP)
}
\keyword{data}
\keyword{NWIS}
\keyword{retrieval}

