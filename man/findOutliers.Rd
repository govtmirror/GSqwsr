\name{findOutliers}
\alias{findOutliers}
\title{Calculates potential outliers based on external studentized residuals}
\usage{
findOutliers(modelReturn, localDT, transformResponse = "lognormal")
}
\arguments{
  \item{localDT}{DTframe that includes all response and
  predictor variables}

  \item{modelReturn}{list returned from censReg}

  \item{transformResponse}{string can be "normal" or
  "lognormal", perhaps try to generalize this more in
  future}
}
\value{
outlier vector of index numbers
}
\description{
Find index of outliers using external studentized
residuals. Outliers are values that have external
studentized residuals greater than 3 or less than negative
3.
}
\examples{
DTComplete <- StLouisDT
UV <- StLouisUV
QWcodes <- StLouisQWcodes
siteINFO <- StLouisInfo
response <- QWcodes$colName[1]
DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
kitchenSink <- createFullFormula(DT,response)
returnPrelim <- prelimModelDev(DT,response,kitchenSink)
modelReturn <- returnPrelim$DT.mod
outlierIndex <- findOutliers(modelReturn,DT)
}
\keyword{residuals}
\keyword{studentized}

