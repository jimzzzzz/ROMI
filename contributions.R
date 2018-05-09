# Contribution code -------------------------------------------------------


# the betas multiplied by the values of the independent vars
# contributions <- sweep(ind.Var, 2, model_beta, "*") * scaling_factor

#better without scaling factor

contributions <- sweep(ind.Var, 2, model_beta, "*")

# defining the input matrix for subsequent work
INPUT <- cbind(pVal, model_beta, t(contributions))

# Defining fitted values, actual and residuals
Fit <- c(NA, NA, apply(as.data.frame(INPUT)[3:dim(INPUT)[2]],2,sum)+model_base)
Actuals <- c(NA, NA ,ds.prep.model$target)
Residuals <- Actuals - Fit

INPUT <- rbind(t(Residuals), t(Fit), t(Actuals), t(c(0, 1, model_base)), INPUT)
rownames(INPUT)[1:4] <- c("Residuals","Fit","Actuals","Moving_Base")
colnames(INPUT)[1:2] <- c("pVal", "model_beta")
