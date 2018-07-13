# model 14 - Indirect digital 3P
# First Regression - Use the variables & parameters from worksheet "Model 12" Step 1a & Run kalman filter. 
# Second Regression - Next, run the regression again - With variables in step 1b.

options(scipen= 999, digits=8)
library(dlm)
library(car)
library(data.table)
library(ggplot2)
library(lattice)
library(knitr)

# ds.prep <- read.csv("Model_Database_2018-01-22_v1.csv")
ds.prep <- read.csv("Model_Database_20180326_Until KW201743_3.csv") # validation done

target <- ds.prep$UM_Digitalind_3P_Sales_Units_OE

# First regression --------------------------------------------------------

#Making a base model - needs to be in the same data set

n<-c(  
  
  # Price & Discount
  
  "UM_AllChan_3P_ListPrice_Dummy"
  ,"UM_AllChan_3P_EffectivePriceRed_Euros"
  
  # Media Spends
  
  , "UM_AllChan_HSIn_OOH_SpendGross"
  , "UM_AllChan_HSIn_Magazines_SpendGross"
  
  # Digital
  , "UM_AllChan_HSIn_PaidSearch_Impressions_Nonbrand_Product"
  , "UM_AllChan_Non_B2B_sum_Impressions"
  , "UM_AllChan_3P_SocialFBInsta_Impressions"
  , "UM_AllChan_AllProd_NonProgTotal_Impressions"
  
  # Commissions

  # Sales channel support
  , "UM_AllChan_3p_Outbound_ReachedContacts"

    # Value proposition change
  , "UM_AllChan_3P_VP5_Stepchange"
  
  # Competition
  , "Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V0"
  , "Comp_AllChan_HSIn_Spend"
  
  # GRP
  , "UM_AllChan_HSIn_Radio_GRP"
  
  # Seasonality
  
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

# Adstock first regression ------------------------------------------------

ds.prep.model$UM_AllChan_HSIn_PaidSearch_Impressions_Nonbrand_Product <- adstock_it(ds.prep.model$UM_AllChan_HSIn_PaidSearch_Impressions_Nonbrand_Product, 0.1, 0.9, 100)
ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions <- adstock_it(ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions, 0.1, 0.4, 1000)
ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions, 0.9, 0.95, 1000)
ds.prep.model$UM_AllChan_3P_SocialFBInsta_Impressions <- adstock_it(ds.prep.model$UM_AllChan_3P_SocialFBInsta_Impressions, 0.1, 0.8, 100)
ds.prep.model$Comp_AllChan_HSIn_Spend <- adstock_it(ds.prep.model$Comp_AllChan_HSIn_Spend, 0.1, 0.6, 1000)
ds.prep.model$UM_AllChan_3p_Outbound_ReachedContacts <- adstock_it(ds.prep.model$UM_AllChan_3p_Outbound_ReachedContacts, 0.1, 0.9, 100)
ds.prep.model$UM_AllChan_HSIn_Radio_GRP <- adstock_it(ds.prep.model$UM_AllChan_HSIn_Radio_GRP, 0.6, 0.1, max(ds.prep.model$UM_AllChan_HSIn_Radio_GRP))
ds.prep.model$UM_AllChan_HSIn_Magazines_SpendGross <- adstock_it(ds.prep.model$UM_AllChan_HSIn_Magazines_SpendGross, 0.8, 0.9, 500)
ds.prep.model$UM_AllChan_HSIn_OOH_SpendGross <- adstock_it(ds.prep.model$UM_AllChan_HSIn_OOH_SpendGross, 0.8, 0.9, 500)
# scalar transformations - none

#  Model code First regression---------------------------------------------

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


# Repeat DLM with VP2 in place of List Price ------------------------------

n<-c(  
  
  # Price & Discount
  
#  "UM_AllChan_3P_ListPrice_Dummy"
  "UM_AllChan_3P_EffectivePriceRed_Euros"
  
  # Media Spends
  
  , "UM_AllChan_HSIn_OOH_SpendGross"
  , "UM_AllChan_HSIn_Magazines_SpendGross"
  
  # Digital
  , "UM_AllChan_HSIn_PaidSearch_Impressions_Nonbrand_Product"
  , "UM_AllChan_Non_B2B_sum_Impressions"
  , "UM_AllChan_3P_SocialFBInsta_Impressions"
  , "UM_AllChan_AllProd_NonProgTotal_Impressions"
  
  # Commissions
  
  # Sales channel support
  , "UM_AllChan_3p_Outbound_ReachedContacts"
  
  # Value proposition change
  , "UM_AllChan_3P_VP5_Stepchange"
  , "UM_AllChan_3P_VP2_Stepchange"
  
  # Competition
  , "Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V0"
  , "Comp_AllChan_HSIn_Spend"
  
  # GRP
  , "UM_AllChan_HSIn_Radio_GRP"
  
  # Seasonality
  
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

# Adstock second regression ------------------------------------------------
ds.prep.model$UM_AllChan_HSIn_PaidSearch_Impressions_Nonbrand_Product <- adstock_it(ds.prep.model$UM_AllChan_HSIn_PaidSearch_Impressions_Nonbrand_Product, 0.1, 0.9, 100)
ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions <- adstock_it(ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions, 0.1, 0.4, 1000)
ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions, 0.9, 0.95, 1000)
ds.prep.model$UM_AllChan_3P_SocialFBInsta_Impressions <- adstock_it(ds.prep.model$UM_AllChan_3P_SocialFBInsta_Impressions, 0.1, 0.8, 100)
ds.prep.model$Comp_AllChan_HSIn_Spend <- adstock_it(ds.prep.model$Comp_AllChan_HSIn_Spend, 0.1, 0.6, 1000)
ds.prep.model$UM_AllChan_3p_Outbound_ReachedContacts <- adstock_it(ds.prep.model$UM_AllChan_3p_Outbound_ReachedContacts, 0.1, 0.9, 100)
ds.prep.model$UM_AllChan_HSIn_Radio_GRP <- adstock_it(ds.prep.model$UM_AllChan_HSIn_Radio_GRP, 0.6, 0.1, max(ds.prep.model$UM_AllChan_HSIn_Radio_GRP))
ds.prep.model$UM_AllChan_HSIn_Magazines_SpendGross <- adstock_it(ds.prep.model$UM_AllChan_HSIn_Magazines_SpendGross, 0.8, 0.9, 500)
ds.prep.model$UM_AllChan_HSIn_OOH_SpendGross <- adstock_it(ds.prep.model$UM_AllChan_HSIn_OOH_SpendGross, 0.8, 0.9, 500)


#  Model code second regression---------------------------------------------

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
