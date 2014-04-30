\name{createFullFormula}
\alias{createFullFormula}
\title{Creates text for the upper bound formula.}
\usage{
  createFullFormula(localDT, responseVariable)
}
\arguments{
  \item{localDT}{dataframe of potential input variables to
  model}

  \item{responseVariable}{string column header of single
  response variable to model}
}
\value{
  upperBoundFormula text string of formula that includes
  all variables and log variables
}
\description{
  Creates text for the upper bound formula. Accepts a
  dataframe DT and the name of the column with the response
  variable. From that, all other columns except datetime
  and decYear (decimal year) are used to create an upper
  bound formula for the stepwise regression. This includes
  all log transformations of columns that have no values
  less than or equal to zero. Log transforms are also not
  allowed on pH data.
}
\examples{
DTComplete <- StLouisDT
UV <- StLouisUV
response <- "Ammonia.N"
DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
upperBoundFormula <- createFullFormula(DT,response)
}
\keyword{formula}

