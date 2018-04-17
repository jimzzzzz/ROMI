scaling_factor <- 100
vLevel <- NULL

complete.db <- ds.prep.model
dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
fitOrder <- 1

# Model definition happens over here
modelMle <- dlmMLE(y = dep.Var,
                   parm = rep(0.0, times = fitOrder + 1),
                   build = GetModel,
                   method = "CG")

# This is checking if we want to fix level variance based on prior information
if(!is.null(vLevel)){
  modelMle$par[2] <- vLevel
}

# Printing the variance for level determined
cat(sprintf("\t Model variances (Input) : %0.3f - par(%0.3f)\n", 
            exp(modelMle$par), modelMle$par))

# Model structure and parameter estimation happens here 
dataModel <- GetModel(modelMle$par)

# Kalman Filter to calculate the betas and Trend
kFilterData <- dlmFilter(dep.Var, dataModel)
kFilterSmooth <- dlmSmooth(kFilterData, dataModel)

# Extract model betas -----------------------------------------------------
# Extract betas and trend from the model
model_beta <- dropFirst(kFilterSmooth$s)[1, 1:dim(ind.Var)[2]] * scaling_factor
names(model_beta) <- colnames(ind.Var)

model_base <- kFilterSmooth$s[, dim(ind.Var)[2] + 1] * scaling_factor

View(model_beta)

# Calculate p-values ------------------------------------------------------

cov <- dlmSvd2var(kFilterSmooth$U.S, kFilterSmooth$D.S)[-1]
width <- t(qnorm(.95) * sqrt(sapply(cov,diag)))[1,1:dim(ind.Var)[2]]

se <- scaling_factor* width/(1.96)
zVal <- model_beta/se
pVal <- pnorm(-abs(zVal))
pVal <- c(0.0, pVal)

View(pVal)