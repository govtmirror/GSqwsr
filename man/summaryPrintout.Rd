\name{summaryPrintout}
\alias{summaryPrintout}
\title{summaryPrintout}
\usage{
summaryPrintout(modelReturn, siteINFO, saveOutput = FALSE,
  fileName = "output.txt")
}
\arguments{
  \item{modelReturn}{censReg model results}

  \item{siteINFO}{dataframe including station name
  (station.nm) and siteID (site.no) (easiestly retrieved
  from dataRetrieval package)}

  \item{saveOutput}{logical}

  \item{fileName}{string full path and file name to save
  to}
}
\description{
Summarize the model results
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
modelReturn <- censReg(paste(response," ~ ", kitchenSink, sep=""), dist="lognormal", data=DT)
summaryPrintout(modelReturn,siteINFO)
}
\keyword{summary}
\keyword{text}

