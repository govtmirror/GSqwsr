\name{resultResidPlots}
\alias{resultResidPlots}
\title{resultResidPlots}
\usage{
  resultResidPlots(localDT, finalModel, siteINFO)
}
\arguments{
  \item{localDT}{dataframe in wide format}

  \item{finalModel}{censReg model results}

  \item{siteINFO}{dataframe including station name
  (station.nm) and siteID (site.no) (easiestly retrieved
  from dataRetrieval package)}
}
\value{
  plot
}
\description{
  Plots summary with residuals plots
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
resultResidPlots(DT,modelReturn,siteINFO)
}
\keyword{scatterplot}

