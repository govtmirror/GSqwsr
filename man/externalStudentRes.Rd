\name{externalStudentRes}
\alias{externalStudentRes}
\title{externalStudentRes}
\usage{
externalStudentRes(localDT, modelCoefList, transformResponse = "lognormal")
}
\arguments{
  \item{localDT}{DTframe that includes all response and
  predictor variables}

  \item{modelCoefList}{list of model coefficients}

  \item{transformResponse}{string can be "normal" or
  "lognormal", perhaps try to generalize this more in
  future}
}
\value{
StRes.all
}
\description{
Compute internally Studentized residuals
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
modelCoef <- modelReturn$PARAML
names(modelCoef) <- c(dimnames(modelReturn$XLCAL)[[2]],"logSigma")
modelCoefList <- list()
modelCoefList[[rownames(attributes(modelReturn$terms)$factors)[1]]] <- modelCoef
externalStudentRes(DT,modelCoefList)
}
\keyword{residuals}
\keyword{studentized}

