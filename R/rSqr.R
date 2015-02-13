#'Calculates R^2 and adjusted R^2 values
#'
#'Calculates R^2 and adjusted R^2 values
#'
#'@param observed number vector of observed data
#'@param resid number vector of residual data
#'@param predicted number vector of predicted data
#'@return df list with rSquare and adjusted rSquare
#'@keywords rSquared
#'@export
#'@examples
#'observed <- runif(10, 5.0, 7.5)
#'resid <- runif(10, 0.1, 0.2)
#'predicted<-1
#'rList <- rSqr(observed, resid, predicted)
rSqr <- function(observed, resid, predicted){
  if(length(resid)==0) resid <- predicted-observed
  n <- length(observed)
  yMean <- mean(observed)
  SStot <- sum((observed-yMean)^2)
  
  SSerr <- sum((resid)^2)
 
  rSquare <- 1-(SSerr/SStot)
  
  VARerr <- SSerr/(n-1-p)
  VARtot <- SStot/(n-1)
  adjRsquare <- 1-(VARerr/VARtot)
  
  df <- list(Rsq=rSquare,adjRsq=adjRsquare)
  return(df)
}