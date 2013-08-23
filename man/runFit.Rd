\name{runFit}
\alias{runFit}
\title{runFit}
\usage{
  runFit(localDT, finalModel,
    transformResponse = "lognormal")
}
\arguments{
  \item{finalModel}{results from censReg}

  \item{localDT}{dataframe of potential input variables to
  model}

  \item{transformResponse}{string can be "normal" or
  "lognormal", perhaps try to generalize this more in
  future}
}
\value{
  evaluat list
}
\description{
  Run the fitting routine
}
\examples{
DTComplete <- DTComplete
UV <- UV
QWcodes <- QWcodes
siteINFO <- siteINFO
response <- QWcodes$colName[1]
DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
kitchenSink <- createFullFormula(DT,response)
returnPrelim <- prelimModelDev(DT,response,kitchenSink)
modelReturn <- returnPrelim$DT.mod
runFit(DT,modelReturn)
}
\keyword{AMLE}
\keyword{fit}

