# model 4
# We have not been able to exactly replicate Truesight results
# Differences particularly with betas of Stepchange variables
# I suspect that their results are based on an earlier iteration of the data

options(scipen= 999, digits=8)
library(dlm)
library(car)
library(data.table)
library(ggplot2)
library(lattice)
library(knitr)

ds.prep <- read.csv("Model_Database_2018-01-22_v1.csv")
ds.prep <- read.csv("Model_Database_20180327_Until KW201743_1.csv")

target <- ds.prep$UM_TeleinSiS_2P_Sales_Units_OE

# First regression --------------------------------------------------------

n<-c(  
  # Price & Discount
  
  "UM_AllChan_2P_ListPrice_Dummy"
  ,"UM_AllChan_2P_EffectivePriceRed_Euros"
  
  # Media Spends
  
  , "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
  
  # Digital
  , "UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product"
  , "UM_AllChan_AllProd_ProgrammaticTot_Impressions"
  , "UM_AllChan_AllProd_NonProgTotal_Impressions"
  
  # Commissions
  
  # Sales channel support
  , "UM_AllChan_2p_Whitemail_Number"
  
  # Value proposition change
  , "UM_AllChan_2P_LosWochos_Stepchange"
  , "UM_AllChan_2P_VP5_Stepchange"
#  , "UM_AllChan_2P_VP2_Stepchange"
  
  # Competition
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V5"
  , "Comp_AllChan_HSIn_Spend"
  
  # GRP

  # Seasonality
  
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

#
# Adstock the data
#

ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross <- adstock_it(ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross, 0.1, 0.6, 100)
ds.prep.model$UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product <- adstock_it(ds.prep.model$UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product, 0.1, 0.9, 500)
ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions, 0.6, 0.9, 500)
ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions, 0.1, 0.9, 500)
ds.prep.model$Comp_AllChan_HSIn_Spend <- adstock_it(ds.prep.model$Comp_AllChan_HSIn_Spend, 0.1, 0.9, 500)

scaling_factor <- 1000
complete.db <- ds.prep.model
dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
vLevel <- NULL
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

# Calculate p-values ------------------------------------------------------

cov <- dlmSvd2var(kFilterSmooth$U.S, kFilterSmooth$D.S)[-1]
width <- t(qnorm(.95) * sqrt(sapply(cov,diag)))[1,1:dim(ind.Var)[2]]

se <- scaling_factor* width/(1.96)
zVal <- model_beta/se
pVal <- pnorm(-abs(zVal))
pVal <- c(0.0, pVal)

View(pVal)

# Second regression --------------------------------------------------------

n<-c(  
  # Price & Discount
  
  "UM_AllChan_2P_ListPrice_Dummy"
  ,"UM_AllChan_2P_EffectivePriceRed_Euros"
  
  # Media Spends
  
  , "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
  
  # Digital
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product" #note Non_Brand is now NonBrand
  , "UM_AllChan_AllProd_ProgrammaticTot_Impressions"
  , "UM_AllChan_AllProd_NonProgTotal_Impressions"
  
  # Commissions
  
  # Sales channel support
  , "UM_AllChan_2p_Whitemail_Number"
  
  # Value proposition change
  , "UM_AllChan_2P_LosWochos_Stepchange"
  , "UM_AllChan_2P_VP5_Stepchange"
  , "UM_AllChan_2P_VP2_Stepchange"
  
  # Competition
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V5"
  , "Comp_AllChan_HSIn_Spend"
  
  # GRP
  
  # Seasonality
  
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

#
# Adstock the data
#

ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross <- adstock_it(ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross, 0.1, 0.6, 100)
ds.prep.model$UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product <- adstock_it(ds.prep.model$UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product, 0.1, 0.9, 500)
ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions, 0.6, 0.9, 500)
ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions, 0.1, 0.9, 500)
ds.prep.model$Comp_AllChan_HSIn_Spend <- adstock_it(ds.prep.model$Comp_AllChan_HSIn_Spend, 0.1, 0.9, 500)

scaling_factor <- 1000
complete.db <- ds.prep.model
dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
vLevel <- NULL
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

# Calculate p-values ------------------------------------------------------

cov <- dlmSvd2var(kFilterSmooth$U.S, kFilterSmooth$D.S)[-1]
width <- t(qnorm(.95) * sqrt(sapply(cov,diag)))[1,1:dim(ind.Var)[2]]

se <- scaling_factor* width/(1.96)
zVal <- model_beta/se
pVal <- pnorm(-abs(zVal))
pVal <- c(0.0, pVal)

View(pVal)

# Secondary regression ----------------------------------------------------

# multiply with the beta of traditional media, leading to the contribution 
# !!! Multiply with the ADSTOCKED TM Spendgross !!!

target <- 242.766322207926 * ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross

independent_n <- c("UM_AllChan_Internet_Radio_SpendGross"
                   ,"UM_AllChan_Internet_TV_SpendGross"
                   ,"UM_AllChan_Internet_TradePress_SpendGross"
                   ,"UM_AllChan_Internet_Magazine_SpendGross"
                   ,"UM_AllChan_Internet_OOH_SpendGross"
                   ,"UM_AllChan_Internet_NewsPaper_SpendGross" 
                   ,"UM_AllChan_Internet_Cinema_SpendGross"
)

independent <- ds.prep[independent_n]

independent$UM_AllChan_Internet_Radio_SpendGross       <- adstock_it(independent$UM_AllChan_Internet_Radio_SpendGross     , 0.1, 0.6, 500)
independent$UM_AllChan_Internet_TV_SpendGross          <- adstock_it(independent$UM_AllChan_Internet_TV_SpendGross        , 0.1, 0.6, 500)
independent$UM_AllChan_Internet_TradePress_SpendGross  <- adstock_it(independent$UM_AllChan_Internet_TradePress_SpendGross, 0.1, 0.6, 100)
independent$UM_AllChan_Internet_Magazine_SpendGross    <- adstock_it(independent$UM_AllChan_Internet_Magazine_SpendGross  , 0.1, 0.6, 500)
independent$UM_AllChan_Internet_OOH_SpendGross         <- adstock_it(independent$UM_AllChan_Internet_OOH_SpendGross       , 0.1, 0.6, 500)
independent$UM_AllChan_Internet_NewsPaper_SpendGross   <- adstock_it(independent$UM_AllChan_Internet_NewsPaper_SpendGross , 0.1, 0.6, 500)
independent$UM_AllChan_Internet_Cinema_SpendGross      <- adstock_it(independent$UM_AllChan_Internet_Cinema_SpendGross    , 0.1, 0.6, 500)

library(stats)

# " -1" in the formula leads to a intercept of 0

linear_model <- lm(target ~ . - 1, data = independent)

# Multipl. with the Beta of traditional media spend

beta_second_model <- linear_model$coefficients 
View(beta_second_model)
pval_second_model <- summary(linear_model)$coefficients[,4]
View(pval_second_model)


