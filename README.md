surrogateRegression: GSqwsr
===========================

Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This software is provided "AS IS".

Installation
------------

	install.packages(c("XML", "lubridate", "akima", "KernSmooth",
		"leaps", "car", "mvtnorm", "digest","relimp", "BSDA", "RODBC",
		"memoise","boot","survival","splines","RColorBrewer","lattice",
		"MASS"),dependencies=TRUE)
	install.packages(c("dataRetrieval","USGSwsBase","USGSwsData",
		"USGSwsGraphs","USGSwsStats","USGSwsQW"), repo="http://usgs-r.github.com")
	install.packages("GSqwsr", repo="http://usgs-r.github.com")
