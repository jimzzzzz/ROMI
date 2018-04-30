# model 8 - indirect digital 2P
# 2 steps: 1) main dlm model, 
# 2) Take the weekly contribution of the Traditional Media from regression in Step 1a as Dependent Variable. 
# Independent Variables & parameters are to be taken from Step2a.  Run an OLS with no intercept.

options(scipen= 999, digits=8)
library(dlm)
library(car)
library(data.table)
library(ggplot2)
library(lattice)
library(knitr)
library(dlm)

# ds.prep <- read.csv("Model_Database_2018-01-22_v1.csv")
ds.prep <- read.csv("Model_Database_20180403_Until KW201743_2.csv")
target <- ds.prep$UM_Digitalind_2P_Sales_Units_OE

n<-c(  
  
  # Price & Discount
  
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  
  # Media Spends
  
  , "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
  
  # Digital
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_T"
  , "UM_AllChan_Non_B2B_sum_Impressions"
  , "UM_AllChan_AllProd_NonProgDisplay_Impressions_T"
  , "UM_AllChan_2P_SocialFBInsta_Impressions_T"
  
  # Commissions
  
  # Sales channel support
  , "UM_AllChan_2P_Outbound_ReachedContacts2"
  
  # Value proposition change

  
  # Competition
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6"
  , "Comp_AllChan_Media_Spend_T"
  
  # GRP
  
  # Seasonality

)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

#
# Adstock the data
#

ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross <- adstock_it(ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross, 0.1, 0.3, 200)
ds.prep.model$UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_T <- adstock_it(ds.prep.model$UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_T, 0.1, 0.5, 100)
ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions <- adstock_it(ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions, 0.1, 0.3, 200)
ds.prep.model$UM_AllChan_AllProd_NonProgDisplay_Impressions_T <- adstock_it(ds.prep.model$UM_AllChan_AllProd_NonProgDisplay_Impressions_T, 0.2, 0.9, 200)
ds.prep.model$UM_AllChan_2P_SocialFBInsta_Impressions_T <- adstock_it(ds.prep.model$UM_AllChan_2P_SocialFBInsta_Impressions_T, 0.4, 0.9, 100)
ds.prep.model$Comp_AllChan_Media_Spend_T <- adstock_it(ds.prep.model$Comp_AllChan_Media_Spend_T, 0.1, 0.7, 100)
ds.prep.model$UM_AllChan_2P_Outbound_ReachedContacts2 <- adstock_it(ds.prep.model$UM_AllChan_2P_Outbound_ReachedContacts2, 0.1, 0.5, 100)

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

target <- 714.138584 * ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross

independent_n <- c("UM_AllChan_Internet_Radio_SpendGross"
                   ,"UM_AllChan_Internet_TV_SpendGross"
                   ,"UM_AllChan_Internet_TradePress_SpendGross"
                   ,"UM_AllChan_Internet_Magazine_SpendGross"
                   ,"UM_AllChan_Internet_OOH_SpendGross"
                   ,"UM_AllChan_Internet_NewsPaper_SpendGross" 
                   ,"UM_AllChan_Internet_Cinema_SpendGross"
)

independent <- ds.prep[independent_n]

independent$UM_AllChan_Internet_Radio_SpendGross       <- adstock_it(independent$UM_AllChan_Internet_Radio_SpendGross     , 0.1, 0.4, 500)
independent$UM_AllChan_Internet_TV_SpendGross          <- adstock_it(independent$UM_AllChan_Internet_TV_SpendGross        , 0.1, 0.4, 500)
independent$UM_AllChan_Internet_TradePress_SpendGross  <- adstock_it(independent$UM_AllChan_Internet_TradePress_SpendGross, 0.1, 0.4, 100)
independent$UM_AllChan_Internet_Magazine_SpendGross    <- adstock_it(independent$UM_AllChan_Internet_Magazine_SpendGross  , 0.1, 0.4, 500)
independent$UM_AllChan_Internet_OOH_SpendGross         <- adstock_it(independent$UM_AllChan_Internet_OOH_SpendGross       , 0.1, 0.4, 500)
independent$UM_AllChan_Internet_NewsPaper_SpendGross   <- adstock_it(independent$UM_AllChan_Internet_NewsPaper_SpendGross , 0.1, 0.4, 500)
independent$UM_AllChan_Internet_Cinema_SpendGross      <- adstock_it(independent$UM_AllChan_Internet_Cinema_SpendGross    , 0.1, 0.4, 500)

library(stats)

# " -1" in the formula leads to a intercept of 0

linear_model8 <- lm(target ~ . - 1, data = independent)

# Multipl. with the Beta of traditional media spend

beta_second_model <- linear_model8$coefficients 
View(beta_second_model)
pval_second_model <- summary(linear_model8)$coefficients[,4]
View(pval_second_model)

# These betas fit
# following code is to get the contributions (not necessary for just checking the model with new data)

gesamt <- sum(target)

# To get exactly the values from the decomp file:
# Split  the contribution of each variable onto "traditional media" and split it up over every week
# Alternative: just multiply the beta with the raw values, this might lead to a worse model!

as.numeric(beta_second_model[names(beta_second_model) == "UM_AllChan_Internet_Radio_SpendGross"]) / sum(as.numeric(beta_second_model) * target[2])

a1 <- (beta_second_model[names(beta_second_model) == "UM_AllChan_Internet_Radio_SpendGross"]      * independent$UM_AllChan_Internet_Radio_SpendGross)
a2 <- (beta_second_model[names(beta_second_model) == "UM_AllChan_Internet_TV_SpendGross"]         * independent$UM_AllChan_Internet_TV_SpendGross)
a3 <- (beta_second_model[names(beta_second_model) == "UM_AllChan_Internet_TradePress_SpendGross"] * independent$UM_AllChan_Internet_TradePress_SpendGross)
a4 <- (beta_second_model[names(beta_second_model) == "UM_AllChan_Internet_Magazine_SpendGross"]   * independent$UM_AllChan_Internet_Magazine_SpendGross)
a5 <- (beta_second_model[names(beta_second_model) == "UM_AllChan_Internet_OOH_SpendGross"]        * independent$UM_AllChan_Internet_OOH_SpendGross)
a6 <- (beta_second_model[names(beta_second_model) == "UM_AllChan_Internet_NewsPaper_SpendGross"]  * independent$UM_AllChan_Internet_NewsPaper_SpendGross)
a7 <- (beta_second_model[names(beta_second_model) == "UM_AllChan_Internet_Cinema_SpendGross"]     * independent$UM_AllChan_Internet_Cinema_SpendGross)

target * (sum(a1) / gesamt )
target * (sum(a2) / gesamt )
target * (sum(a3) / gesamt )
target * (sum(a4) / gesamt )
target * (sum(a5) / gesamt )
target * (sum(a6) / gesamt )
target * (sum(a7) / gesamt )

# What happened here: Take the residuals and split them equally on every variable
# An alternativ would be to distribute the residual based on the contribution of every variable in one week (not implemented here!!)

all_together <- cbind(a1, a2, a3, a4, a5, a6, a7)

rowSums(all_together) - target
target

korrektur <- (rowSums(all_together) - target) / 7

# Now a1 - a7 sum up to the TM contribution

a1 <- a1 - korrektur
a2 <- a2 - korrektur
a3 <- a3 - korrektur
a4 <- a4 - korrektur
a5 <- a5 - korrektur
a6 <- a6 - korrektur
a7 <- a7 - korrektur

