\name{predictionPlot}
\alias{predictionPlot}
\title{predictionPlot}
\usage{
predictionPlot(localUV, localDT, finalModel, transformResponse = "lognormal",
  siteINFO)
}
\arguments{
  \item{localUV}{dataframe of unit values}

  \item{localDT}{dataframe in wide format}

  \item{finalModel}{censReg model results}

  \item{transformResponse}{string can be "lognormal" or
  "normal", perhaps try to generalize this more in future}

  \item{siteINFO}{dataframe including station name
  (station.nm) and siteID (site.no) (easiestly retrieved
  from dataRetrieval package)}
}
\value{
plot
}
\description{
Plots summary plots
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
predictionPlot(UV,DT,modelReturn,siteINFO=siteINFO)
}
\keyword{scatterplot}

