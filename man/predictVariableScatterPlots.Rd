\name{predictVariableScatterPlots}
\alias{predictVariableScatterPlots}
\title{predictVariableScatterPlots}
\usage{
  predictVariableScatterPlots(localDT, responseVariable,
    transformResponse = "lognormal")
}
\arguments{
  \item{localDT}{dataframe in wide format}

  \item{responseVariable}{character}

  \item{transformResponse}{string can be "normal" or
  "lognormal", perhaps try to generalize this more in
  future}
}
\value{
  plot
}
\description{
  Plots scatterplot of predict variables.
}
\examples{
DTComplete <- DTComplete
QWcodes <- QWcodes
response <- QWcodes$colName[1]
DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
predictVariableScatterPlots(DT,response)
}
\keyword{scatterplot}

