# Model 4 - Inbound and SIS 2P

# Define target ------------------------------------------

target <- ds.prep$UM_TeleinSiS_2P_Sales_Units_OE

# Run adstocks ------------------------------------------------------------

source("adstock all.R")

# Use model_variable_selection.R to select variables to enter here

# nm is list of non-adstocked
# ns is list of adstocked

# This is the original selection of non-adstocked variables
nm <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V5"
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
  , "UM_AllChan_2p_Whitemail_Number"
  , "UM_AllChan_2P_LosWochos_Stepchange"
  , "UM_AllChan_2P_VP5_Stepchange"
)

ns<-c(
  "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product"
  , "UM_AllChan_AllProd_ProgrammaticTot_Impressions"
  , "UM_AllChan_AllProd_NonProgTotal_Impressions"
  , "Comp_AllChan_HSIn_Spend"
  
)


# Use the following line if no adstocked variable included yet
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm])

# Use following line if non-adstocked and adstocked variables being used
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

# Run all chosen variables through model
scaling_factor <- 1000
vLevel <- NULL
source("model code.R")

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

# In the original model 8, there was a post model adjustment to remove the max from the V6 variable
# Remove max - Variable Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6

a <- (ds.prep.model$Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6)
a_max <- max(a)
a_final <- a - a_max
ds.prep.model$Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6 <- a_final

# the betas multiplied by the values of the independent vars
# contributions <- sweep(ind.Var, 2, model_beta, "*") * scaling_factor
contributions <- sweep(ind.Var, 2, model_beta, "*")

# defining fitted values 
Fit <- c(NA, NA, apply(as.data.frame(INPUT)[3:dim(INPUT)[2]],2,sum)+model_base)

# defining Actuals 
Actuals <- c(NA, NA ,ds.prep.model$target)

# defining Residuals 
Residuals <- Actuals - Fit

# defining the input matrix for subsequent work
INPUT <- cbind(pVal, model_beta, t(contributions))
INPUT <- rbind(t(Residuals), t(Fit), t(Actuals), t(c(0, 1, model_base)), INPUT)
rownames(INPUT)[1:4] <- c("Residuals","Fit","Actuals","Moving_Base")
colnames(INPUT)[1:2] <- c("pVal", "model_beta")

View(INPUT)

# In the original model 8, there was a post model adjustment to remove the max from the V6 variable
# Remove max - Variable Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6

a <- (ds.prep$Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6)
a_max <- max(a)
a_final <- a - a_max