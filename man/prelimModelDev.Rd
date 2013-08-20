\name{prelimModelDev}
\alias{prelimModelDev}
\title{prelimModelDev}
\usage{
  prelimModelDev(localDT, responseVariable,
    upperBoundFormula, k = "AIC",
    transformResponse = "lognormal")
}
\arguments{
  \item{localDT}{dataframe of potential input variables to
  model}

  \item{responseVariable}{string column header of single
  response variable to model}

  \item{upperBoundFormula}{string of upper bound for model
  generation}

  \item{k}{string either "AIC", "BIC", or value of the
  multiple of the number of degrees of freedom used for the
  penalty.}

  \item{transformResponse}{string can be "normal" or
  "lognormal", perhaps try to generalize this more in
  future}
}
\value{
  retVal list of modelStuff, steps, localDT.mod
}
\description{
  Run stepwise regression and generate simplifed model
  output.
}
\examples{
DTComplete <- DTComplete
UV <- UV
QWcodes <- QWcodes
response <- QWcodes$colName[1]
DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
kitchenSink <- createFullFormula(DT,response)
returnPrelim <- prelimModelDev(DT,response,kitchenSink)
steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
}
\keyword{residuals}
\keyword{studentized}

