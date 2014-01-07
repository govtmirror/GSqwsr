\name{predictionPlot}
\alias{predictionPlot}
\title{Prediction plot of model results over time.}
\usage{
predictionPlot(localUV, localDT, finalModel, transformResponse = "lognormal",
  siteINFO)
}
\arguments{
  \item{localUV}{dataframe of unit values, which needs to
  includes one column called "datetime" that is in
  POSIXct.}

  \item{localDT}{dataframe in wide format, which needs to
  includes one column called "datetime" that is in
  POSIXct.}

  \item{finalModel}{censReg model results}

  \item{transformResponse}{string can be "lognormal" or
  "normal", perhaps try to generalize this more in future}

  \item{siteINFO}{dataframe including station name
  (station.nm) and siteID (site.no) (easiestly retrieved
  from dataRetrieval package)}
}
\description{
Using unit value data, returns a plot of the model results.
Predictions are bias corrected if transform response is set
to "lognormal". Predictions are plotted as a blue line,
actual sample measurements are plotted as solid red points.
Censored values are distinguished with line segments, and
the points are . The plot title is generated from the
siteINFO dataframe, which needs columns station.nm and
site.no.
}
\examples{
DTComplete <- StLouisDT
UV <- StLouisUV
response <- "Ammonia.N"
siteINFO <- StLouisInfo
DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
kitchenSink <- createFullFormula(DT,response)
returnPrelim <- prelimModelDev(DT,response,kitchenSink)
modelReturn <- returnPrelim$DT.mod
predictionPlot(UV,DT,modelReturn,siteINFO=siteINFO)
}
\keyword{scatterplot}

