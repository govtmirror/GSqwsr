\name{createFullDT}
\alias{createFullDT}
\title{createFullDT}
\usage{
createFullDT(formulaToUse, localDT)
}
\arguments{
  \item{formulaToUse}{formula}

  \item{localDT}{dataframe of potential input variables to
  model}
}
\value{
fullDTList list of
}
\description{
Get log and interaction columns
}
\examples{
DTComplete <- DTComplete
UV <- UV
QWcodes <- QWcodes
response <- QWcodes$colName[1]
DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
basicFormula <- "Flow + log(Flow)"
newDT <- createFullDT(basicFormula,DT)
}
\keyword{transforms}

