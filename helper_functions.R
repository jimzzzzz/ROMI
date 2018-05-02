
adstock_it <- function(vector, learn, decay, scalar){
  n <- length(vector)
  output <- rep(0,n)
  vector <- (vector / max(vector)) * scalar 
  
  for(i in 1 : length(output)){
    
    Decay <- -decay
    HRF   <- learn
    if(i == 1) {zaehler <- 1} else {zaehler <- 1 - output[i-1] * exp(Decay)}
    nenner  <- exp(vector[i] / 100 * HRF )                                      # Teile durch 100 da es als Prozente angegeben werden soll! 
    output[i] <- 1 - (zaehler/nenner)
  }
  return(output)
}

best_adstock <- function(var, dep_var, B){
  # a function that loops through a grid with possible adstock parameters 
  # and chooses the best ones based on their correlation with a given variable - dep_var
  # 
  # Prameters: 
  #   var - the variable that would be adstocked
  #   dep_var - the variable to which the correlation between the transformed var would be measured
  #   B - dataframe resulting from expand grid with 3 columns like this one:
  #       expand.grid(learn = seq(0, 1, 0.2), decay = seq(0,1, 0.2), scalar = c(100,500,1000))
  # 
  # Returns:
  #   a list with the following elements:
  #   adstocked_vector_max - the transformed vector with best parameters
  #   results - a dataframe containing info about the correlations for the different parameters
  #   adstock_params - a vector with the parameters that maximise the correlation with dep_var - learn, decay, scalar
  # 
  
  # handler for variables with zero variance
  if(var(var) == 0){
    return(list(adstocked_vector_max = var, 
                results = "no variance for this variable",
                adstock_params = "no variance for this variable")
    )
  }
  
  # # Creating empty dataframe
  results <- data.frame(i = numeric(0),
                        Adstock = character(0),
                        Score = numeric(0),
                        Cor = numeric(0),
                        PValue = numeric(0))
  
  adstocked_vector_final <- var
  max_score <- -1
  
  for(i in 1 :nrow(B)){
    # print(i)
    vector <- var
    learn  <- B$learn[i]
    decay  <- B$decay[i]
    scalar <- B$scalar[i]
    adstock_name <- paste(learn, decay, scalar, sep="_")
    adstocked_vector <- adstock_it(vector, learn, decay, scalar)
    
    res <- cor.test(dep_var, adstocked_vector, method = "pearson") 
    
    output <- data.frame(i, 
                         adstock_name, 
                         res$statistic, 
                         res$estimate,
                         res$p.value)
    
    colnames(output) <- c("i", "Adstock", "Score", "Cor", "PValue")
    results <- rbind(results, output)
    
    # choosing the adstock with the best score
    if(!is.na(res$statistic)){
      if(abs(res$statistic) >= max_score){
        max_score <- abs(res$statistic)
        adstocked_vector_max <- adstocked_vector
        adstock_name_max <- adstock_name
      }
    }
  }
  
  adstock_params <- strsplit(adstock_name_max, "_")
  adstock_params <- as.numeric(unlist(adstock_params))
  names(adstock_params) <- c("learn", "decay", "scalar")
  adstock_params <- as.list(adstock_params)
  
  return(list(adstocked_vector_max = adstocked_vector_max, 
              results = results,
              adstock_params = adstock_params)
  )
}

DLMModel <- function(ds.prep.model, 
                     scaling_factor = 100,
                     vLevel = NULL # used for prior model variance
){
  
  complete.db <- ds.prep.model
  
  dep.Var <- complete.db[,1]/scaling_factor
  ind.Var <- complete.db[,2 : ncol(complete.db)]
  
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
  if(!is.null(vLevel)){
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
  
  se <- scaling_factor* width/(1.96)
  zVal <- model_beta/se
  pVal <- pnorm(-abs(zVal))
  
  # the betas multiplied by the values of the independent vars
  contributions <- sweep(ind.Var, 2, model_beta, "*") 
  
  # defining the input matrix for subsequent work
  INPUT <- cbind(pVal, model_beta, t(contributions))
  INPUT <- rbind(t(c(0, 1, model_base)), INPUT)
  
  DLMModel <- list(
    INPUT = INPUT,
    model_base = model_base,
    model_beta = model_beta,
    pVal = pVal,
    contributions = contributions
  )
  
  class(DLMModel) <- "DLMModel"
  
  return(DLMModel)
}









