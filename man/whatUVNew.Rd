\name{whatUVNew}
\alias{whatUVNew}
\title{whatUVNew}
\usage{
  whatUVNew(siteNumber, service = "uv")
}
\arguments{
  \item{siteNumber}{string USGS site number.  This is
  usually an 8 digit number}

  \item{service}{string defaults to "uv" (unit values),
  other options are "qw" (water quality) and "dv" (daily
  values)}
}
\value{
  retval dataframe with all information found in the
  expanded site file
}
\description{
  Imports a table of available parameters, period of
  record, and count. There is also an option to load the
  long parameter names and additional information on the
  parameters with longNames=TRUE.
}
\examples{
site <- "04027000"
uv <-whatUVNew(site)
}
\keyword{USGS}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}

