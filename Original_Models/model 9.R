# model 9 - D2D + Outbound telemarketing 3P
# First Regression - Use the variables & parameters from worksheet "Model 09" Step 1a & Run kalman filter. 
# Second Regression - Next, run the regression again - With variables in step 1b

options(scipen= 999, digits=8)
library(dlm)
library(car)
library(data.table)
library(ggplot2)
library(lattice)
library(knitr)
library(dlm)

# ds.prep <- read.csv("Model_Database_2018-01-22_v1.csv")
ds.prep <- read.csv("Model_Database_20180327_Until KW201743_1.csv") # validated
target <- ds.prep$UM_D2DTeleout_3P_Sales_Units_OE

# First regression --------------------------------------------------------

n<-c(  
  
  # Price & Discount
  
  "UM_AllChan_3P_ListPrice_Dummy"
  
  # Media Spends &GRP
  
  , "UM_AllChan_HSIn_Radio_GRP"
  , "UM_AllChan_HSIn_Magazines_SpendGross"
  
  # Digital
  , "UM_AllChan_Non_B2B_sum_Impressions"

  # Commissions
  , "UM_D2DTeleout_3P_HSIn_CPO2_SpendPerOE_K2"
  , "UM_D2DTeleout_3P_HSIn_CPO3_Spend_T2"
  
  # Sales channel support
  , "UM_D2DTeleout_AllProd_SalesSupport_Spend_Total"
  
  # Direct marketing
  , "UM_AllChan_3p_Outbound_ReachedContacts"
  
  # Competition
  , "Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V1"

  # Seasonality
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_NationalorFederalHolidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

# Adstock first regression ------------------------------------------------

ds.prep.model$UM_AllChan_HSIn_Radio_GRP <- adstock_it(ds.prep.model$UM_AllChan_HSIn_Radio_GRP, 0.1, 0.9, max(ds.prep.model$UM_AllChan_HSIn_Radio_GRP))
ds.prep.model$UM_AllChan_HSIn_Magazines_SpendGross <- adstock_it(ds.prep.model$UM_AllChan_HSIn_Magazines_SpendGross, 0.1, 0.9, 500)
ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions <- adstock_it(ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions, 0.1, 0.95, 300)
# scalar transformations
ds.prep.model$UM_D2DTeleout_AllProd_SalesSupport_Spend_Total <- ds.prep.model$UM_D2DTeleout_AllProd_SalesSupport_Spend_Total/100
ds.prep.model$UM_D2DTeleout_3P_HSIn_CPO3_Spend_T2 <- ds.prep.model$UM_D2DTeleout_3P_HSIn_CPO3_Spend_T2/100

#  Model code First regression---------------------------------------------

scaling_factor <- 1000
complete.db <- ds.prep.model
dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
vLevel <- -5
fitOrder <- 1

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

# Model betas First regression--------------------------------------------
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

# Second regression -------------------------------------------------------

n<-c(  
  
  # Price & Discount
  
#  "UM_AllChan_3P_ListPrice_Dummy"
  "UM_AllChan_3P_VP2_Stepchange"
  
  # Media Spends &GRP
  
  , "UM_AllChan_HSIn_Radio_GRP"
  , "UM_AllChan_HSIn_Magazines_SpendGross"
  
  # Digital
  , "UM_AllChan_Non_B2B_sum_Impressions"
  
  # Commissions
  , "UM_D2DTeleout_3P_HSIn_CPO2_SpendPerOE_K2"
  , "UM_D2DTeleout_3P_HSIn_CPO3_Spend_T2"
  
  # Sales channel support
  , "UM_D2DTeleout_AllProd_SalesSupport_Spend_Total"
  
  # Direct marketing
  , "UM_AllChan_3p_Outbound_ReachedContacts"
  
  # Competition
  , "Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V1"
  
  # Seasonality
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_NationalorFederalHolidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

# Adstock the data
#

ds.prep.model$UM_AllChan_HSIn_Radio_GRP <- adstock_it(ds.prep.model$UM_AllChan_HSIn_Radio_GRP, 0.1, 0.9, max(ds.prep.model$UM_AllChan_HSIn_Radio_GRP))
ds.prep.model$UM_AllChan_HSIn_Magazines_SpendGross <- adstock_it(ds.prep.model$UM_AllChan_HSIn_Magazines_SpendGross, 0.1, 0.9, 500)
ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions <- adstock_it(ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions, 0.1, 0.95, 300)
# scalar transformations
ds.prep.model$UM_D2DTeleout_AllProd_SalesSupport_Spend_Total <- ds.prep.model$UM_D2DTeleout_AllProd_SalesSupport_Spend_Total/100
ds.prep.model$UM_D2DTeleout_3P_HSIn_CPO3_Spend_T2 <- ds.prep.model$UM_D2DTeleout_3P_HSIn_CPO3_Spend_T2/100

scaling_factor <- 1000
complete.db <- ds.prep.model
dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
vLevel <- -5
fitOrder <- 1

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

# Extract betas and trend from the model
model_beta <- dropFirst(kFilterSmooth$s)[1, 1:dim(ind.Var)[2]] * scaling_factor
names(model_beta) <- colnames(ind.Var)
model_base <- kFilterSmooth$s[, dim(ind.Var)[2] + 1] * scaling_factor
View(model_beta)

cov <- dlmSvd2var(kFilterSmooth$U.S, kFilterSmooth$D.S)[-1]
width <- t(qnorm(.95) * sqrt(sapply(cov,diag)))[1,1:dim(ind.Var)[2]]

se <- scaling_factor* width/(1.96)
zVal <- model_beta/se
pVal <- pnorm(-abs(zVal))
pVal <- c(0.0, pVal)

View(pVal)
