\name{runPred}
\alias{runPred}
\title{runPred}
\usage{
  runPred(localUV, localDT, finalModel,
    transformResponse = "lognormal", dfReady = TRUE)
}
\arguments{
  \item{localUV}{dataframe of unit values}

  \item{localDT}{dataframe in wide format}

  \item{finalModel}{censReg model results}

  \item{transformResponse}{string can be "normal" or
  "lognormal", perhaps try to generalize this more in
  future}

  \item{dfReady}{logical if the localDT dataframe already
  includes time/log columns}
}
\value{
  predictedReturn list
}
\description{
  Run the model
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
}
\keyword{AMLE}
\keyword{fit}

