\name{createFormulaFromDF}
\alias{createFormulaFromDF}
\title{Creates text for upperBoundFormula from a dataframe}
\usage{
createFormulaFromDF(df)
}
\arguments{
  \item{df}{dataframe}
}
\value{
modelFormula text string of formula that includes variables
and log variables in df
}
\description{
Creates text for upperBoundFormula prelimModelDev formula.
Takes a dataframe (df), one column is named 'Scalar', and
one is named 'variableNames', the rest are the available
parameters. Each row in the data contains one of the
available parameters. For every 1 in the 'Scalar' column,
the parameter is added to the formula. If a 1 is in the
matrix of parameters, interaction terms are created.
}
\examples{
parameters <- sampleParameters
formulaText <- createFormulaFromDF(parameters)
}
\keyword{creation}
\keyword{formula}

