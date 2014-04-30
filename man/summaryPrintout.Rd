\name{summaryPrintout}
\alias{summaryPrintout}
\title{Text summarization of the model results}
\usage{
  summaryPrintout(modelReturn, siteINFO,
    saveOutput = FALSE, fileName = "output.txt")
}
\arguments{
  \item{modelReturn}{censReg model results}

  \item{siteINFO}{dataframe including station name
  (station.nm) and siteID (site.no) (easiestly retrieved
  from dataRetrieval package)}

  \item{saveOutput}{logical defaults to FALSE. Set to TRUE
  if an output file is desired}

  \item{fileName}{string full path and file name to save
  output}
}
\description{
  Summarize the model results
}
\examples{
DTComplete <- StLouisDT
UV <- StLouisUV
response <- "Ammonia.N"
siteINFO <- StLouisInfo
DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
kitchenSink <- createFullFormula(DT,response)
modelReturn <- censReg(paste(response," ~ ", kitchenSink, sep=""), dist="lognormal", data=DT)
summaryPrintout(modelReturn,siteINFO)
}
\keyword{summary}
\keyword{text}

