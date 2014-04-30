\name{createModelEquation}
\alias{createModelEquation}
\title{Create the model equation including coefficients.}
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
  Create the model equation including coefficients. Accepts
  the results of a censReg regression model and turns it
  into an text equation suitable for plot captions.
}
\examples{
DTComplete <- StLouisDT
UV <- StLouisUV
response <- "Ammonia.N"
DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
kitchenSink <- createFullFormula(DT,response)
returnPrelim <- prelimModelDev(DT,response,kitchenSink)
modelReturn <- returnPrelim$DT.mod
createModelEquation(modelReturn)
}
\keyword{equation}

