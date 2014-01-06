\name{analyzeSteps}
\alias{analyzeSteps}
\title{Plot showing improvement in models during stepwise regression}
\usage{
analyzeSteps(steps, responseVariable, siteINFO, xCorner = 0.05,
  yCorner = 0.45)
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
Plot showing improvement in model choices during the course
of the stepwise regression. Five criteria are shown:
Correlation, slope, RMSE, PRESS, and AIC. For corrolation,
see \code{?cor}. RMSE is the root-mean-squared error of the
difference between observed values and the predicted values
(see \code{?rmse}). The PRESS statistic (prediction error
sum of squares) stastic (Helsel and Hirsch, 2002) is
calculated using external studentized residuals (see
\code{?externalStudentRes}). AIC is the Akaike information
criterion. When running the \code{prelimModelDev} function,
specifying "AIC" or "BIC" (Bayesian information criterion)
will change the value of k used to calculate the AIC value.
For an "AIC" stepwise regression, \code{k=2}, for a "BIC"
stepwise regression,
\code{k=log(length(responseVariable))}. Finally 'slope' is
simply the slope of a linear regression of observed versus
predicted (a rough measure of model accuracy). Minimizing
AIC is the only factor that has an effect on the model
outcome.
}
\examples{
DTComplete <- StLouisDT
UV <- StLouisUV
siteINFO <- StLouisInfo
response <- "Ammonia.N"
DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
kitchenSink <- createFullFormula(DT,response)
returnPrelim <- prelimModelDev(DT,response,kitchenSink,"AIC")
steps <- returnPrelim$steps
analyzeSteps(steps,response,siteINFO,xCorner = 0.05, yCorner=0.45)
}
\references{
Helsel, D.R. and Hirsch, R.M., 2002, Statistical methods in
water resources: U.S. Geological Survey Techniques of
Water-Resources Investigations, book 4, chap. A3, 522 p.
}
\keyword{scatterplot}

