#'Stepwise regression using censReg function
#'
#'@description Run stepwise regression and generate simplifed model output. See \code{?censReg} for information on
#'the censored data regression function. For an "AIC" stepwise regression, set \code{k="AIC"}, this internally defines \code{k=2}.
#'For a "BIC" stepwise regression, set \code{k="BIC"}, which internally defintes'\code{k=log(length(responseVariable))}.
#'A number for k (the multiple of the number of degrees of freedom used for the penalty) can also be specified.
#'
#'This function has a unique feature that can be turned off by setting autoSinCos to \code{FALSE}. If there are columns in the DT data that include sine and cosine of the decimal year 
#'(as indicated by column names sinDY and cosDY) then when one of those columns is selected in the stepwise regression,
#'the other column automatically is included in the next step. To disable this feature, set autoSinCos to \code{FALSE}.
#'
#'@param localDT dataframe of potential input variables to model
#'@param responseVariable string column header of single response variable to model
#'@param upperBoundFormula string of upper bound for model generation
#'@param k string either "AIC", "BIC", or value of the multiple of the number of degrees of freedom used for the penalty.
#'@param transformResponse string can be "normal" or "lognormal", perhaps try to generalize this more in future
#'@param autoSinCos logical, turns off the feature to automatically include sinDY and cosDY if either is picked in the stepwise regression.
#'The default is TRUE, which includes the feature.
#'@return The output is a named list that can be split into 3 dataframes: modelInformation, steps, and DT.mod.
#'DT.mod is generated from the output of the censReg function. steps is a dataframe that shows information
#'for each of the steps taken during the stepwise regression. modelInformation is a dataframe that contains information 
#'for the final model, including the names of the chosen parameters, their coefficients, standard error, p-value, and standard coefficient.
#'See \code{?censReg} for information on how these were calculated.
#'@keywords studentized residuals
#'@export
#'@examples
#' DTComplete <- StLouisDT
#' UV <- StLouisUV
#' response <- "Ammonia.N"
#' DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
#' DT <- na.omit(DT)
#' kitchenSink <- createFullFormula(DT,response)
#' returnPrelim <- prelimModelDev(DT,response,kitchenSink)
#' steps <- returnPrelim$steps
#' modelResult <- returnPrelim$modelInformation
#' modelReturn <- returnPrelim$DT.mod
prelimModelDev <- function(localDT,responseVariable,
                           upperBoundFormula,
                           k="BIC",
                           transformResponse="lognormal",
                           autoSinCos=TRUE){
  
  if ("AIC" == k){
    k <- 2
  } else if ("BIC" == k){
    k <- log(length(localDT[,responseVariable]))
  }
  
  distribution <- transformResponse
  formulaToUse <- paste(responseVariable," ~ 1",sep="")
 
  DT.mod <- do.call("stepAIC", args=list(object=call("censReg",
                                formulaToUse,
                                data=localDT,
                                dist=distribution),
                           scope=list(lower= ~ 1, upper=formula(paste("~ ",upperBoundFormula,sep=""))), k=k))
  
  pathToModel <- DT.mod$anova
  
  steps <- with(pathToModel, data.frame(step=Step, AIC=AIC,
             Deviance=Deviance, Resid.Dev=get('Resid. Dev'), Resid.Df=get('Resid. Df')))
  
  if(!autoSinCos){
    if(any(grepl("sinDY",as.character(steps$step)) | grepl("cosDY",as.character(steps$step)))){
      
      if (!(any(grepl("sinDY",as.character(steps$step))) & any(grepl("cosDY",as.character(steps$step))))){
    
        if(any(grepl("sinDY",as.character(steps$step)))){
          firstIndex <- which(grepl("sinDY",as.character(steps$step)))[1]
          lowerScope <- paste(as.character(steps$step[2:firstIndex]),collapse=" ")
          lowerScope <- paste("~ ", substring(lowerScope, 2, nchar(lowerScope)), " + cosDY",sep="")
        } else {
          firstIndex <- which(grepl("cosDY",as.character(steps$step)))[1]
          lowerScope <- paste(as.character(steps$step[2:firstIndex]),collapse=" ")
          lowerScope <- paste("~ ", substring(lowerScope, 2, nchar(lowerScope)), " + sinDY",sep="")
        }
        
        formulaToUseNew <- paste(responseVariable,lowerScope)
        
        DT.mod <- do.call("stepAIC", args=list(object=call("censReg",
                                 formulaToUseNew,
                                 data=localDT,
                                 dist=distribution),
                     scope=list(lower= formula(lowerScope), upper=formula(paste("~ ",upperBoundFormula,sep=""))), k=k))
    
        pathToModel2 <- DT.mod$anova
        
        steps2 <- with(pathToModel2, data.frame(step=Step, AIC=AIC,
                    Deviance=Deviance, Resid.Dev=get('Resid. Dev'), Resid.Df=get('Resid. Df')))
        steps2$step <- as.character(steps2$step)
        
        if(any(grepl("sinDY",as.character(steps$step)))){
          steps2$step[1] <- "+ cosDY"
        } else {
          steps2$step[1] <- "+ sinDY"
        }
        
              
        steps <- rbind(steps[1:firstIndex,],steps2)
      }
    }
  }
  
  steps$Correlation <- rep(NA,nrow(steps))
  steps$Slope <- rep(NA,nrow(steps))
  
  steps$Res.St.Error <- rep(NA,nrow(steps))
  steps$PRESS <- rep(NA,nrow(steps))
  responseValue <- localDT[,responseVariable]@.Data[,2]
  steps$scope <- as.character(steps$step)
  steps$response <- rep(responseVariable,nrow(steps))
  
  
  lmFormula <- "pred ~ obs"
  if ("lognormal" == transformResponse){
    lmFormula <- "log10(pred) ~ log10(obs)"
  }
  
  if(nrow(steps) > 1){
    cat("Analyzing steps (total=",nrow(steps),"): ")
    for(j in 2:nrow(steps)){
      cat(j,"\n")
      steps$scope[j] <- paste(steps$scope[j-1],steps$scope[j],sep=" ")
      
      formulaToUse <- substring(steps$scope[j],3, nchar(steps$scope[j]))
      formulaToUse <- paste(responseVariable, formulaToUse,sep=" ~ ")
      modelReturnStep <- do.call("censReg", list(formulaToUse, data=localDT, dist=distribution))
      
      modelCoef <- modelReturnStep$PARAML
      names(modelCoef) <- c(dimnames(modelReturnStep$XLCAL)[[2]],"logSigma")
      modelCoefList <- list()
      modelCoefList[[steps$response[1]]] <- modelCoef
      
      StRes.all.extReturn <- externalStudentRes(localDT, modelCoefList,transformResponse)
      steps$PRESS[j] <- StRes.all.extReturn$PRESS
            
      df <- data.frame(obs=responseValue, pred=modelReturnStep$YPRED)
      lineFit <- do.call("lm", list(lmFormula, data=df))
      corStep <- cor(df$obs, df$pred)
      steps$Slope[j] <- lineFit$coefficients[2]
      steps$Correlation[j] <- corStep
      steps$Res.St.Error[j] <- rmse(modelReturnStep)
      
      
    }
  }
  
  StCoef <- with(DT.mod, PARAML/STDDEV)
  modelStuff <- with(DT.mod, data.frame(names=c(dimnames(DT.mod$XLCAL)[[2]],"logSigma"),
                                        coefficients=PARAML, 
                                        StdError=STDDEV, 
                                        pValue=PVAL,
                                        StCoef=StCoef
  ))

  retVal <- list(modelInformation=modelStuff, steps=steps, DT.mod=DT.mod)
  return(retVal)
}
  
  