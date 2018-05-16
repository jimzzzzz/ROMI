
adstock_it <- function(vector, learn, decay, scalar){
  n <- length(vector)
  output <- rep(0,n)
  vector <- (vector / max(vector)) * scalar 
  
  for (i in 1:length(output)) {
    
    Decay <- -decay
    HRF   <- learn
    if (i == 1) {zaehler <- 1} else {zaehler <- 1 - output[i - 1] * exp(Decay)}
    nenner  <- exp(vector[i] / 100 * HRF )                                      # Teile durch 100 da es als Prozente angegeben werden soll! 
    output[i] <- 1 - (zaehler/nenner)
  }
  return(output)
}

GetModel <- function(parm){
  
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
  #   adstock_params - the parameters that maximise the correlation with dep_var
  # 
  
  # handler for variables with zero variance
  if (var(var) == 0) {
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
  
  for (i in 1:nrow(B)) {
    # print(i)
    vector <- var
    learn  <- B$learn[i]
    decay  <- B$decay[i]
    scalar <- B$scalar[i]
    adstock_name <- paste(learn, decay, scalar, sep = "_")
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
    if (!is.na(res$statistic)) {
      if (abs(res$statistic) >= max_score) {
        max_score <- abs(res$statistic)
        adstocked_vector_max <- adstocked_vector
        adstock_name_max <- adstock_name
      }
    }
  }
  
  return(list(adstocked_vector_max = adstocked_vector_max, 
              results = results,
              adstock_params = adstock_name_max)
  )
}

create.formula <- function(y.name, x.vector){
  facts <- x.vector[1]
  if (length(x.vector) != 1) {
    for (i in 2:length(x.vector)) {
      facts <- paste0(facts, " + ", x.vector[i])
    }
  }
  
  temp.formula <- formula(paste0(y.name, " ~ ", facts))
  return(temp.formula)
}
