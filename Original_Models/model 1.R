# model 1 - 1P Digital Sales
# First Regression - Use the variables & parameters from worksheet "Model 01" Step 1a & Run kalman filter. 
# Second Regression - Next, run the regression again - With variables in step 1b.
# Final Model will Include the result of regression in step 1a (With List Price in Model). The Beta & P Value  of VP2 will come from regression in Step1b.
# The contributions of List Price from regression in Step 1a to be divided in List Price & VP2 in the ratios of their respective contributions.
# Take the weekly contribution of the Traditional Media from regression in Step 1a as Dependent Variable. Independent Variables & parameters are to be taken from Step2a.  Run an OLS with no intercept.
# Similarly do for Paidsearch_Nonbrand_product & Social_FB_Insta with inputs from Step 2b.

options(scipen= 999, digits=8)
library(dlm)
library(car)
library(data.table)
library(ggplot2)
library(lattice)
library(knitr)

ds.prep <- read.csv("Model_Database_2018-01-22_v1.csv")
ds.prep <- read.csv("Model_Database_20180403_Until KW201743_1.csv")

target <- ds.prep$UM_Digital_1P_Sales_Units_OE

# First regression --------------------------------------------------------
  
n<-c(  
  
  # Price & Discount
  
  "UM_AllChan_1P_ListPrice_Dummy"

  # Media Spends
  
  , "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
  
  # Digital
  , "UM_AllChan_1P_PaidSearch_NonBrand_Product"
  , "UM_AllChan_AllProd_ProgrammaticTot_Impressions"
  , "UM_AllChan_AllProd_NonProgTotal_Impressions"   
  
  # Commissions

  # Sales channel support

  # Value proposition change
  , "UM_AllChan_1P_VP5_Stepchange"
  , "UM_Digitaldir_1P_TP134_EuroValue"
  , "UM_Digitaldir_1P_TP50_EuroValue"
  
  # Competition
  , "Comp_AllChan_HSIn_Spend"
  
  # GRP
  
  # Seasonality
  , "UM_AllChan_AllProd_DecJanHolidaysXmas_NY_GeoWeightedDays"
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays" 
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

# Adstock first regression ------------------------------------------------
ds.prep.model$UM_AllChan_1P_PaidSearch_NonBrand_Product      <- adstock_it(ds.prep.model$UM_AllChan_1P_PaidSearch_NonBrand_Product , 0.1, 0.9, 500)
ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions , 0.1, 0.9, 1000)
ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions    <- adstock_it(ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions    , 0.1, 0.9, 100)
ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross    <- adstock_it(ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross    , 0.1, 0.9, 500)
# scalar adjustment
ds.prep.model$Comp_AllChan_HSIn_Spend <- ds.prep.model$Comp_AllChan_HSIn_Spend/100

#  Model code first regression---------------------------------------------

scaling_factor <- 1000
complete.db <- ds.prep.model
dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
vLevel <- -4
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

# Second regression - take out list price dummy, introduce VP2 stepchange ----------------------------

n<-c(  
  
  # Price & Discount
  
  # Media Spends
  
  "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
  
  # Digital
  , "UM_AllChan_1P_PaidSearch_NonBrand_Product"
  , "UM_AllChan_AllProd_ProgrammaticTot_Impressions"
  , "UM_AllChan_AllProd_NonProgTotal_Impressions"   
  
  # Commissions
  
  # Sales channel support
  
  # Value proposition change
  , "UM_AllChan_1P_VP2_Stepchange"
  , "UM_AllChan_1P_VP5_Stepchange"
  , "UM_Digitaldir_1P_TP134_EuroValue"
  , "UM_Digitaldir_1P_TP50_EuroValue"
  
  # Competition
  , "Comp_AllChan_HSIn_Spend"
  
  # GRP
  
  # Seasonality
  , "UM_AllChan_AllProd_DecJanHolidaysXmas_NY_GeoWeightedDays"
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays" 
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

# Adstock second regression ------------------------------------------------
ds.prep.model$UM_AllChan_1P_PaidSearch_NonBrand_Product     <- adstock_it(ds.prep.model$UM_AllChan_1P_PaidSearch_NonBrand_Product      , 0.1, 0.9, 500)
ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions , 0.1, 0.9, 1000)
ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions    <- adstock_it(ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions    , 0.1, 0.9, 100)
ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross    <- adstock_it(ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross    , 0.1, 0.9, 500)
# scalar adjustment
ds.prep.model$Comp_AllChan_HSIn_Spend <- ds.prep.model$Comp_AllChan_HSIn_Spend/100

#  Model code second regression---------------------------------------------

scaling_factor <- 1000
complete.db <- ds.prep.model
dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
vLevel <- -4
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

# Calculate p-values second regression ------------------------------------------------------

cov <- dlmSvd2var(kFilterSmooth$U.S, kFilterSmooth$D.S)[-1]
width <- t(qnorm(.95) * sqrt(sapply(cov,diag)))[1,1:dim(ind.Var)[2]]

se <- scaling_factor* width/(1.96)
zVal <- model_beta/se
pVal <- pnorm(-abs(zVal))
pVal <- c(0.0, pVal)

View(pVal)


# PaidSearch regression ---------------------------------------------------

target_ps <- 721.03140769817 * ds.prep.model$UM_AllChan_1P_PaidSearch_NonBrand_Product
# the variable used in the main regression is the total paid search
# a part of that is the SocialFBInsta and we create a variable for the rest

ds.prep$UM_AllChan_1P_PaidSearch_NonBrand_Product_T <- ds.prep$UM_AllChan_1P_PaidSearch_NonBrand_Product - ds.prep$UM_AllChan_1P_SocialFBInsta_Impressions
ps_n <-c("UM_AllChan_1P_SocialFBInsta_Impressions", "UM_AllChan_1P_PaidSearch_NonBrand_Product_T")
independent_ps <- ds.prep[ps_n]
independent_ps$UM_AllChan_1P_PaidSearch_NonBrand_Product_T <- adstock_it(independent_ps$UM_AllChan_1P_PaidSearch_NonBrand_Product_T,0.1,0.9,500)
independent_ps$UM_AllChan_1P_SocialFBInsta_Impressions <- adstock_it(independent_ps$UM_AllChan_1P_SocialFBInsta_Impressions,0.1,0.9,500)

linear_model <- lm(target_ps ~ . - 1, data = independent_ps)

# Multipl. with the Beta of paid search spend

beta_second_model <- linear_model$coefficients 
View(beta_second_model)
pval_second_model <- summary(linear_model)$coefficients[,4]
View(pval_second_model)


# Media spend regression ----------------------------------------------------

# multiply with the beta of traditional media, leading to the contribution 
# !!! Multiply with the ADSTOCKED TM Spendgross !!!

target_ms <- 82.61112295176 * ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross

ms_n <- c("UM_AllChan_Internet_Radio_SpendGross"
                   ,"UM_AllChan_Internet_TV_SpendGross"
                   ,"UM_AllChan_Internet_TradePress_SpendGross"
                   ,"UM_AllChan_Internet_Magazine_SpendGross"
                   ,"UM_AllChan_Internet_OOH_SpendGross"
                   ,"UM_AllChan_Internet_NewsPaper_SpendGross" 
                   ,"UM_AllChan_Internet_Cinema_SpendGross"
)

independent_ms <- ds.prep[ms_n]

independent_ms$UM_AllChan_Internet_Radio_SpendGross       <- adstock_it(independent_ms$UM_AllChan_Internet_Radio_SpendGross     , 0.1, 0.9, 500)
independent_ms$UM_AllChan_Internet_TV_SpendGross          <- adstock_it(independent_ms$UM_AllChan_Internet_TV_SpendGross        , 0.1, 0.9, 500)
independent_ms$UM_AllChan_Internet_TradePress_SpendGross  <- adstock_it(independent_ms$UM_AllChan_Internet_TradePress_SpendGross, 0.1, 0.9, 500)
independent_ms$UM_AllChan_Internet_Magazine_SpendGross    <- adstock_it(independent_ms$UM_AllChan_Internet_Magazine_SpendGross  , 0.1, 0.9, 500)
independent_ms$UM_AllChan_Internet_OOH_SpendGross         <- adstock_it(independent_ms$UM_AllChan_Internet_OOH_SpendGross       , 0.1, 0.9, 500)
independent_ms$UM_AllChan_Internet_NewsPaper_SpendGross   <- adstock_it(independent_ms$UM_AllChan_Internet_NewsPaper_SpendGross , 0.1, 0.9, 500)
independent_ms$UM_AllChan_Internet_Cinema_SpendGross      <- adstock_it(independent_ms$UM_AllChan_Internet_Cinema_SpendGross    , 0.1, 0.9, 500)

library(stats)

# " -1" in the formula leads to a intercept of 0

linear_model <- lm(target_ms ~ . - 1, data = independent_ms)

# Multipl. with the Beta of traditional media spend

beta_second_model <- linear_model$coefficients 
View(beta_second_model)
pval_second_model <- summary(linear_model)$coefficients[,4]
View(pval_second_model)


