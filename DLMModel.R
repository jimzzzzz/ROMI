
DLMModel <- function(ds.prep.model, 
                     scaling_factor = 100,
                     vLevel = NULL # used for prior model variance
                     ){

  
  complete.db <- ds.prep.model
  
  dep.Var <- complete.db[,1]/scaling_factor
  ind.Var <- complete.db[,2:ncol(complete.db)]
  
  # This determines the number of unobserved components that we want to have in the model
  #  In absence of not so significant seasonality component, we can fix level variance to 
  # capture seasonal effects; This is in conjunction with dummy variables used for the same
  fitOrder <- 1
  
  GetModel <- function(parm){
    # this is the same function as in the original codes but with ind.Var
    
    
    # Build the model
    modReg <- dlmModReg(ind.Var,
                        addInt = FALSE,
                        dV = 0,
                        dW = rep(0,  times = dim(ind.Var)[2]))
    modTrend <- dlmModPoly(fitOrder)
    dataModel <- modReg + modTrend
    diag(dataModel$V) <- c(exp(parm[1]))
    diag(dataModel$W) <- c(rep(0,
                               times = dim(ind.Var)[2]),
                           exp(parm[2:(fitOrder + 1)]))
    return(dataModel)
  }
  
  modelMle <- dlmMLE(y = dep.Var,
                     parm = rep(0.0, times = fitOrder + 1),
                     build = GetModel,
                     method = "CG")
  
  # This is checking if we want to fix level variance based on prior information
  if (!is.null(vLevel)) {
    modelMle$par[2] <- vLevel
  }
  
  dataModel <- GetModel(modelMle$par)
  
  # Kalman Filter to calculate the betas and Trend
  kFilterData <- dlmFilter(dep.Var, dataModel)
  kFilterSmooth <- dlmSmooth(kFilterData, dataModel)
  
  # Extract betas and trend from the model
  model_beta <- dropFirst(kFilterSmooth$s)[1, 1:dim(ind.Var)[2]] * scaling_factor
  names(model_beta) <- colnames(ind.Var)
  
  model_base <- dropFirst(kFilterSmooth$s)[, dim(ind.Var)[2] + 1] * scaling_factor
  
  # Pvalues calculation
  cov <- dlmSvd2var(kFilterSmooth$U.S, kFilterSmooth$D.S)[-1]
  width <- t(qnorm(.95) * sqrt(sapply(cov,diag)))[1,1:dim(ind.Var)[2]]
  
  se <- scaling_factor * width/(1.96)
  zVal <- model_beta/se
  pVal <- pnorm(-abs(zVal))
  
  # the betas multiplied by the values of the independent vars
  # contributions <- sweep(ind.Var, 2, model_beta, "*")
  contributions <- sweep(ind.Var, 2, model_beta, "*") * scaling_factor
  
  
  # defining fitted values 
  Fit <- c(NA, NA, apply(as.data.frame(Model$INPUT)[3:dim(Model$INPUT)[2]],2,sum))
  
  # defining Actuals 
  Actuals <- c(NA, NA ,ds.prep.model$target)
  
  # defining Residuals 
  Residuals <- Actuals - Fit
  
  
  # defining the input matrix for subsequent work
  INPUT <- cbind(pVal, model_beta, t(contributions))
  INPUT <- rbind(t(Actuals), t(Fit), t(Residuals), t(c(0, 1, model_base)), INPUT)
  rownames(INPUT)[1:4] <- c("Actuals","Fit","Residuals","Moving_Base")
  colnames(INPUT)[1:2] <- c("pVal", "model_beta")
  
  DLMModel <- list(
    INPUT = INPUT,
    model_base = model_base,
    model_beta = model_beta,
    pVal = pVal,
    contributions = contributions,
    fit = as.vector(Fit),
    actuals = as.vector(Actuals),
    residuals = as.vector(Residuals)
    )
  
  class(DLMModel) <- "DLMModel"
  
  return(DLMModel)
}







