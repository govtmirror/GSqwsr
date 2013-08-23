#'predictVariableScatterPlots
#'
#'Plots scatterplot of predict variables.
#'
#'@param localDT dataframe in wide format
#'@param responseVariable character
#'@param transformResponse string can be "normal" or "lognormal", perhaps try to generalize this more in future
#'@return plot
#'@keywords scatterplot
#'@export
#'@examples
#' DTComplete <- DTComplete
#' QWcodes <- QWcodes
#' response <- QWcodes$colName[1]
#' DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
#' DT <- na.omit(DT)
#' predictVariableScatterPlots(DT,response)
predictVariableScatterPlots <- function(localDT,responseVariable,transformResponse="lognormal"){
  
  explanvar <- names(localDT)[-which(names(localDT) %in% responseVariable)]
  explanvar <- explanvar[which(explanvar != "datetime")]
  explanvar <- explanvar[which(explanvar != "decYear")]
  explanvar <- explanvar[which(explanvar != "sinDY")]
  explanvar <- explanvar[which(explanvar != "cosDY")]
  
  linpath <- localDT[,responseVariable]@.Data[,2]
  rho <- cor(cbind(linpath,localDT[,explanvar]), use="complete.obs", method="spearman")[2:(length(explanvar)+1)]
  explanvar.rho <- paste(explanvar,": rho=",round(rho,2),sep="")
  names(explanvar.rho) <- explanvar

  # Could just call long DFResponse:
  localDTLong <- longDFResponse(localDT,explanvar,responseVariable)
  y <- localDTLong$response
  x <- localDTLong$value
  Grp <- localDTLong$variable
  
  p <- xyplot(y~x|Grp,
              main="",
              xlab="",
              ylab="",
              scales = list(y = if ("lognormal" == transformResponse) list(log=TRUE)  else list(relation = 'free'),
                            x = list(relation = 'free')),
              strip=strip.custom(par.strip.text=list(cex=0.75)))
  dimnames(p)[[1]] <- explanvar.rho[dimnames(p)[[1]]]
  if ("lognormal" == transformResponse){
    logText <- "Log"
  } else {
    logText <- ""
  }
  p <- update(p + layer(panel.quantile(x, 
                                       y, tau = c(0.5), superpose = TRUE)), 
              auto.key = list(text = paste("Linear response variables,",logText, responseVariable, c(50), "% quantile"), 
                              points = FALSE, lines = TRUE))
  
  print(p)
  
  lpath <- log(localDT[,responseVariable]@.Data[,2])
  rho <- cor(cbind(lpath,localDT[,explanvar]), use="complete.obs", method="spearman")[2:(length(explanvar)+1)]
  explanvar.rho <- paste(explanvar,": rho=",round(rho,2),sep="")
  names(explanvar.rho) <- explanvar
  
  p <- xyplot(y~x|Grp,
              main="",
              xlab="",
              ylab="",
              scales = list(y = if ("lognormal" == transformResponse) list(log=TRUE)  else list(relation = 'free'),
                            x = list(log=TRUE,relation = 'free')),
              strip=strip.custom(par.strip.text=list(cex=0.75)))
  dimnames(p)[[1]] <- explanvar.rho[dimnames(p)[[1]]]

  p <- update(p + layer(panel.quantile(x, 
                                       y, tau = c(0.5), superpose = TRUE)), 
              auto.key = list(text = paste("Log response variables,", logText, responseVariable, c(50), "% quantile"), 
                              points = FALSE, lines = TRUE))
  
  print(p)
  
}