\name{plotSteps}
\alias{plotSteps}
\title{plotSteps}
\usage{
  plotSteps(steps, localDT,
    transformResponse = "lognormal")
}
\arguments{
  \item{steps}{dataframe}

  \item{localDT}{dataframe of potential input variables to
  model}

  \item{transformResponse}{ring can be "normal" or
  "lognormal", perhaps try to generalize this more in
  future}
}
\value{
  retVal list of modelStuff, steps, DT.mod
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
plotSteps(steps,DT)
}
\keyword{plot}

