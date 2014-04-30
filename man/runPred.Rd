\name{runPred}
\alias{runPred}
\title{Run the model prediction}
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
  Run the model prediction. Takes unit value data (UV) and
  model results to calculate the predicted values. If
  transformResponse is set to "lognormal", a bias
  correction is included.
}
\examples{
DTComplete <- StLouisDT
response <- "Ammonia.N"
UV <- StLouisUV
siteINFO <- StLouisInfo
DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
kitchenSink <- createFullFormula(DT,response)
returnPrelim <- prelimModelDev(DT,response,kitchenSink,"BIC","lognormal")
modelReturn <- returnPrelim$DT.mod
predictedReturn <- runPred(UV, DT, modelReturn,dfReady=FALSE)
}
\keyword{AMLE}
\keyword{fit}

