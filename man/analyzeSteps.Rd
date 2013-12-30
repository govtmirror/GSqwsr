\name{analyzeSteps}
\alias{analyzeSteps}
\title{analyzeSteps}
\usage{
analyzeSteps(steps, responseVariable, siteINFO, xCorner = 0.6,
  yCorner = 0.35)
}
\arguments{
  \item{steps}{dataframe}

  \item{responseVariable}{string column header of single
  response variable to model}

  \item{xCorner}{number represents the x-upper-left corner
  of legend in \% plot area}

  \item{yCorner}{number represents the y-upper-left corner
  of legend in \% plot area}

  \item{siteINFO}{dataframe including station name
  (station.nm) and siteID (site.no) (easiestly retrieved
  from dataRetrieval package)}
}
\value{
plot
}
\description{
Plot showing improvment in model choices
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
steps <- returnPrelim$steps
analyzeSteps(steps,response,siteINFO)
}
\keyword{scatterplot}

