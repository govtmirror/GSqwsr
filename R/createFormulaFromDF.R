#'Creates text for upperBoundFormula from a dataframe
#'
#'Creates text for upperBoundFormula prelimModelDev formula. Takes a dataframe (df), one column
#'is named 'Scalar', and one is named 'variableNames', the rest are the available parameters. Each row in the data
#'contains one of the available parameters. For every 1 in the 'Scalar' column, the parameter is
#'added to the formula. If a 1 is in the matrix of parameters, interaction terms are created.
#'
#'@param df dataframe
#'@return modelFormula text string of formula that includes variables and log variables in df
#'@keywords formula creation
#'@export
#'@examples
#'parameters <- sampleParameters
#'formulaText <- createFormulaFromDF(parameters)
createFormulaFromDF <- function(df){
  
  scalarVariables <- NULL
  scalarVariables <- as.character(df$variableNames[which(1 == df$Scalar)])
  scalarVariables <- paste(scalarVariables,collapse=" + ")
  
  interactionSet <- df[,-2]
  interactionVariables <- ""
  
  modelFormula <- scalarVariables
  
  for (i in colnames(interactionSet)){
    
    var1 <- as.character(interactionSet$variableNames[which(1 == interactionSet[[i]])])
    
    if(substring(i, 1, 4) == "log."){
      i <- paste("log(", substring(i, 5, (nchar(i)-1)), ")",sep="")
    }
    
    if(length(var1) > 0){
      vars <- paste(i,var1,sep=":")
      vars <- paste(vars,collapse=" + ")
      interactionVariables <- paste(interactionVariables,vars,sep=" + ")
    }
    
  }
  
  if (nchar(interactionVariables) > 0){
    interactionVariables <- substring(interactionVariables,4,nchar(interactionVariables))
    modelFormula <- interactionVariables
  }
  
  if (nchar(interactionVariables) > 0 & nchar(scalarVariables) > 0){
    modelFormula <- paste(scalarVariables, interactionVariables, sep= " + ")
  }
  
  if(any(grepl("sinDY",modelFormula) | grepl("cosDY",modelFormula))){
    if(!any(grepl("sinDY",modelFormula) & grepl("cosDY",modelFormula))){
      warning("Attempting to create model with only sinDY or cosDY, not both")
      
    }    
  }
  
  return(modelFormula)
}