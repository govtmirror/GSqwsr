\name{createModelEquation}
\alias{createModelEquation}
\title{createModelEquation}
\usage{
  createModelEquation(modelReturn)
}
\arguments{
  \item{modelReturn}{censReg model results}
}
\value{
  combo string
}
\description{
  Create the model equation including coefficients.
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
createModelEquation(modelReturn)
}
\keyword{equation}

