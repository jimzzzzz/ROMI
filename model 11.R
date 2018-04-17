# model 11 - Direct retail 3P
# Use the variables & parameters from the Worksheet - "Model 11" & Run kalman filter. 
# Secondary regression for trad media

options(scipen= 999, digits=8)
library(dlm)
library(car)
library(data.table)
library(ggplot2)
library(lattice)
library(knitr)

# ds.prep <- read.csv("Model_Database_2018-01-22_v1.csv")
ds.prep <- read.csv("Model_Database_20180327_Until KW201743_1.csv")

target <- ds.prep$UM_Retaildir_3P_Sales_Units_OE

# First regression --------------------------------------------------------

#Making a base model - needs to be in the same data set

n<-c(  
  
  # Price & Discount
  
  "UM_AllChan_3P_ListPrice_Dummy"
  ,"UM_AllChan_3P_EffectivePriceRed_Euros"
  
  # Media Spends
  
  , "UM_HSIn_AllProd_TraditionalMedia_SpendGross"

  # Digital

  # Commissions
  , "UM_Retaildir_3P_HSIn_CPO1b_2_SpendPerOE_N"
  , "UM_Retaildir_3P_HSIn_CPO3_Spend_T"
  
  # Sales channel support
  , "UM_AllChan_2p3p_Whitemail_Number"

  # Value proposition change
  , "UM_AllChan_2P_ScarcityIndicator_StepChange"
  , "UM_AllChan_3P_VP5_Stepchange"
  , "UM_AllChan_3P_VP2_StepchangeInitial"
  
  # Competition
  , "Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V0"
  , "Comp_AllChan_HSIn_TV_GRP"
  
  # GRP

  # Seasonality
  
  , "UM_AllChan_AllProd_NationalorFederalHolidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

# Adstock first regression ------------------------------------------------

ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross <- adstock_it(ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross, 0.1, 0.28, 50)
ds.prep.model$UM_AllChan_3P_VP2_StepchangeInitial <- adstock_it(ds.prep.model$UM_AllChan_3P_VP2_StepchangeInitial, 0.1, 0.1, max(ds.prep.model$UM_AllChan_3P_VP2_StepchangeInitial))
ds.prep.model$UM_AllChan_2p3p_Whitemail_Number <- adstock_it(ds.prep.model$UM_AllChan_2p3p_Whitemail_Number, 0.1, 0.9, 500)
# scalar transformations
ds.prep.model$UM_Retaildir_3P_HSIn_CPO3_Spend_T <- ds.prep.model$UM_Retaildir_3P_HSIn_CPO3_Spend_T/500


#  Model code first regression---------------------------------------------

scaling_factor <- 100
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

target <- 396.549844870 * ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross

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

