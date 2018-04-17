# model 13 - Direct digital 3P
# There are three regressions using Kalman Filter, as in sheet Model 13 step 1a, 1b & 1c. 
# After the first this next step is the Combining the results. 
# First Combine the Traditional Media with List Price. 
# Then Combine Traditional Media with VP 2. 
#

options(scipen= 999, digits=8)
library(dlm)
library(car)
library(data.table)
library(ggplot2)
library(lattice)
library(knitr)

ds.prep <- read.csv("Model_Database_2018-01-22_v1.csv")
ds.prep <- read.csv("Model_Database_20180403_Until KW201743_2.csv")

n<-c( 
  "UM_Digitaldir_3P_Sales_Units_OE"
    ,"UM_AllChan_3P_ListPrice_Dummy"
  ,"UM_HSIn_AllProd_TraditionalMedia_SpendGross"
  ,"Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4"
  ,"Comp_AllChan_TV_Internet_Spend"
  ,"Comp_AllChan_TV_Magazines_Spend"
)

ds.prep.model<-ds.prep[,names(ds.prep) %in% n]

##
# Adstock the data
# The Information which scalar/decay/HRF should be used is taken from the "decomp file"
##

ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross <- adstock_it(ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross , 0.33, 0.99, 2000)
ds.prep.model$Comp_AllChan_TV_Internet_Spend              <- adstock_it(ds.prep.model$Comp_AllChan_TV_Internet_Spend              , 0.60, 0.80, 300)
ds.prep.model$Comp_AllChan_TV_Magazines_Spend             <- adstock_it(ds.prep.model$Comp_AllChan_TV_Magazines_Spend             , 0.76, 0.10, 300)
#ds.prep.model$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4       <- adstock_it(ds.prep.model$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4   , 0.5, 0.5, 200)

# Do the model

# Fitting code ----
scaling_factor <- 100
complete.db <- ds.prep.model

dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
#vLevel <- NULL
fitOrder <- 1
vLevel <- -12

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

# Extract betas and trend from the model
model_beta <- dropFirst(kFilterSmooth$s)[1, 1:dim(ind.Var)[2]] * scaling_factor
names(model_beta) <- colnames(ind.Var)

model_base <- kFilterSmooth$s[, dim(ind.Var)[2] + 1] * scaling_factor

View(model_beta)

beta_tm <- as.numeric(model_beta[names(model_beta) == "UM_HSIn_AllProd_TraditionalMedia_SpendGross"])

#
# traditional media contribution
# Important: Here you can see that the beta of UM_HSIn_AllProd_TraditionalMedia_SpendGross is 35.244627
# Now the beta has to be multiplied with the "raw support"
#

# !!! Multiply with the ADSTOCKED TM Spendgross !!!

trad_media_contr <- beta_tm * ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross

#
# trad_media_contr looks fine since the contribution exactly fits the summed contribution of traditional media spends from the decomp file - as expected)
# The vector "trad_media_contr" will be of interest in the OLS / secondary Model (Step 2)
#

#
# Start Step 1b and 1c
# Step´s are nearly the same as in 1a. Just take different variables
# This step will be repeated from here on for step 1b AND 1c
#


# Here the variables "UM_AllChan_3P_ListPrice_Dummy" and "UM_AllChan_3P_VP2_Stepchange" will be split. 
# For 1b, let "UM_AllChan_3P_ListPrice_Dummy" in and put "UM_AllChan_3P_VP2_Stepchange" out. For 1c, do it vice versa.
#

n<-c(  
  "UM_Digitaldir_3P_Sales_Units_OE"
  
  ,"UM_AllChan_3P_ListPrice_Dummy"
  ,"UM_AllChan_Non_B2B_sum_Impressions"
  ,"UM_AllChan_AllProd_NonProgDisplay_Impressions"
  ,"UM_AllChan_3P_Paidsearch_Discount_Weighted"
  ,"UM_AllChan_3P_SocialFBInsta_Impressions"
  ,"UM_AllChan_MSI_PostDiscount15_Dummy_T"
  ,"UM_AllChan_3P_ScarcityIndicator_StepChange"
  ,"UM_AllChan_3P_VP5_Stepchange"
  ,"Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4"
  ,"Comp_AllChan_TV_Internet_Spend"
  ,"Comp_AllChan_TV_Magazines_Spend"
  ,"UM_HSIn_AllProd_TraditionalMedia_SpendGross" # we delete this one later on, need it to get the "minus_TM" variable!
  #,"UM_AllChan_3P_VP2_Stepchange"
)

ds.prep.model<-ds.prep[,names(ds.prep) %in% n]

# Adjust the model: substract traditional media contribution from dependent variable

ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross <- adstock_it(ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross , 0.33, 0.99, 2000)
ds.prep.model$trad_media_contr <- beta_tm * ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross
ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross <- NULL
ds.prep.model$UM_Digitaldir_3P_Sales_Units_OE_Minus_TM <- ds.prep.model$UM_Digitaldir_3P_Sales_Units_OE - ds.prep.model$trad_media_contr


##
# Adstock the data
##

ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions            <- adstock_it(ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions            , 0.10, 0.90, 50)
ds.prep.model$UM_AllChan_AllProd_NonProgDisplay_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_NonProgDisplay_Impressions , 0.24, 0.39, 200)
ds.prep.model$UM_AllChan_3P_Paidsearch_Discount_Weighted    <- adstock_it(ds.prep.model$UM_AllChan_3P_Paidsearch_Discount_Weighted    , 0.90, 0.90, 400)
ds.prep.model$UM_AllChan_3P_SocialFBInsta_Impressions      <- adstock_it(ds.prep.model$UM_AllChan_3P_SocialFBInsta_Impressions      , 0.90, 0.27, 100)
ds.prep.model$Comp_AllChan_TV_Internet_Spend                <- adstock_it(ds.prep.model$Comp_AllChan_TV_Internet_Spend                , 0.60, 0.80, 300)
ds.prep.model$Comp_AllChan_TV_Magazines_Spend               <- adstock_it(ds.prep.model$Comp_AllChan_TV_Magazines_Spend               , 0.76, 0.10, 300)
# scalars
# ds.prep.model$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4 <- ds.prep.model$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4/200
#ds.prep.model$UM_AllChan_3P_ListPrice_Dummy <- ds.prep.model$UM_AllChan_3P_ListPrice_Dummy/300


#
# Do the model
#

# Fitting code ----
scaling_factor <- 100
complete.db <- ds.prep.model

complete.db$UM_Digitaldir_3P_Sales_Units_OE <- NULL
complete.db$trad_media_contr <- NULL

dep.Var <- complete.db[,c("UM_Digitaldir_3P_Sales_Units_OE_Minus_TM")]/scaling_factor
ind.Var <- complete.db
ind.Var$UM_Digitaldir_3P_Sales_Units_OE_Minus_TM <- NULL


#vLevel <- NULL
fitOrder <- 1
vLevel <- -1

#
# But (for example) the variable ...V4 needs to be "remove max"
# "post model adjustment"
#

a <- (ind.Var$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4)
a_max <- max(a)
a_final <- a - a_max
ind.Var$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4_T <- (ind.Var$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4 / a_max) * 200 # This "200" is seen as scalar in the Modelling Process file from avesh
ind.Var$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4 <- NULL

#a <- (ind.Var$UM_AllChan_3P_ListPrice_Dummy)
#a_max <- max(a)
#a_final <- a - a_max
#ind.Var$UM_AllChan_3P_ListPrice_Dummy_T <- (ind.Var$UM_AllChan_3P_ListPrice_Dummy / a_max) * 300 # This "300" is seen as scalar in the Modelling Process file from avesh
#ind.Var$UM_AllChan_3P_ListPrice_Dummy <- NULL


#
#
#


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

# At this point the betas are perfect except the list price dummy - something to do with scaling I guess
# Remark: These beta´s look fine according to nitesh
#

#
# Adjust the  VP2 / ListPrice_Dummy 
#
n<-c(  
  "UM_Digitaldir_3P_Sales_Units_OE"
  
#  ,"UM_AllChan_3P_ListPrice_Dummy"
  ,"UM_AllChan_Non_B2B_sum_Impressions"
  ,"UM_AllChan_AllProd_NonProgDisplay_Impressions"
  ,"UM_AllChan_3P_Paidsearch_Discount_Weighted"
#  ,"UM_AllChan_3P_SocialFBInsta_Impressions"
  ,"UM_AllChan_MSI_PostDiscount15_Dummy_T"
  ,"UM_AllChan_3P_ScarcityIndicator_StepChange"
  ,"UM_AllChan_3P_VP5_Stepchange"
  ,"Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4"
  ,"Comp_AllChan_TV_Internet_Spend"
  ,"Comp_AllChan_TV_Magazines_Spend"
  ,"UM_HSIn_AllProd_TraditionalMedia_SpendGross" # we delete this one later on, need it to get the "minus_TM" variable!
  ,"UM_AllChan_3P_VP2_Stepchange"
)

ds.prep.model<-ds.prep[,names(ds.prep) %in% n]

# Adjust the model: substract traditional media contribution from dependent variable

ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross <- adstock_it(ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross , 0.33, 0.99, 2000)
ds.prep.model$trad_media_contr <- beta_tm * ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross
ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross <- NULL
ds.prep.model$UM_Digitaldir_3P_Sales_Units_OE_Minus_TM <- ds.prep.model$UM_Digitaldir_3P_Sales_Units_OE - ds.prep.model$trad_media_contr


##
# Adstock the data
##

ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions            <- adstock_it(ds.prep.model$UM_AllChan_Non_B2B_sum_Impressions            , 0.10, 0.90, 50)
ds.prep.model$UM_AllChan_AllProd_NonProgDisplay_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_NonProgDisplay_Impressions , 0.24, 0.39, 200)
ds.prep.model$UM_AllChan_3P_Paidsearch_Discount_Weighted    <- adstock_it(ds.prep.model$UM_AllChan_3P_Paidsearch_Discount_Weighted    , 0.90, 0.90, 400)
#ds.prep.model$UM_AllChan_3P_SocialFBInsta_Impressions      <- adstock_it(ds.prep.model$UM_AllChan_3P_SocialFBInsta_Impressions      , 0.90, 0.27, 100)
ds.prep.model$Comp_AllChan_TV_Internet_Spend                <- adstock_it(ds.prep.model$Comp_AllChan_TV_Internet_Spend                , 0.60, 0.80, 300)
ds.prep.model$Comp_AllChan_TV_Magazines_Spend               <- adstock_it(ds.prep.model$Comp_AllChan_TV_Magazines_Spend               , 0.76, 0.10, 300)
# scalars
# ds.prep.model$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4 <- ds.prep.model$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4/200
#ds.prep.model$UM_AllChan_3P_ListPrice_Dummy <- ds.prep.model$UM_AllChan_3P_ListPrice_Dummy/300


#
# Do the model
#

# Fitting code ----
scaling_factor <- 100
complete.db <- ds.prep.model

complete.db$UM_Digitaldir_3P_Sales_Units_OE <- NULL
complete.db$trad_media_contr <- NULL

dep.Var <- complete.db[,c("UM_Digitaldir_3P_Sales_Units_OE_Minus_TM")]/scaling_factor
ind.Var <- complete.db
ind.Var$UM_Digitaldir_3P_Sales_Units_OE_Minus_TM <- NULL


#vLevel <- NULL
fitOrder <- 1
vLevel <- -1

#
# But (for example) the variable ...V4 needs to be "remove max"
# "post model adjustment"
#

a <- (ind.Var$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4)
a_max <- max(a)
a_final <- a - a_max
ind.Var$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4_T <- (ind.Var$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4 / a_max) * 200 # This "200" is seen as scalar in the Modelling Process file from avesh
ind.Var$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4 <- NULL

#a <- (ind.Var$UM_AllChan_3P_ListPrice_Dummy)
#a_max <- max(a)
#a_final <- a - a_max
#ind.Var$UM_AllChan_3P_ListPrice_Dummy_T <- (ind.Var$UM_AllChan_3P_ListPrice_Dummy / a_max) * 300 # This "300" is seen as scalar in the Modelling Process file from avesh
#ind.Var$UM_AllChan_3P_ListPrice_Dummy <- NULL


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
#
# The betas for listpricedummy and VP2 are too big by eaxctly the same proportion
#
# Build secondary Model (OLS for traditional media)
#

#
# Model Step 2a
#

n<-c(  # Zielvariable
  
  "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
  
  ,"UM_AllChan_Internet_Radio_SpendGross"
  ,"UM_AllChan_Internet_TV_SpendGross"
  ,"UM_AllChan_Internet_TradePress_SpendGross"
  ,"UM_AllChan_Internet_Magazine_SpendGross"
  ,"UM_AllChan_Internet_OOH_SpendGross"
  ,"UM_AllChan_Internet_NewsPaper_SpendGross"
  ,"UM_AllChan_Internet_Cinema_SpendGross"
  
)

ds.prep.model<-ds.prep[,names(ds.prep) %in% n]

ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross <- adstock_it(ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross , 0.33, 0.99, 2000)

ds.prep.model$trad_media_contr <- beta_tm * ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross
ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross <- NULL

target <- ds.prep.model$trad_media_contr

independent_n <- c("UM_AllChan_Internet_Radio_SpendGross"
                   ,"UM_AllChan_Internet_TV_SpendGross"
                   ,"UM_AllChan_Internet_TradePress_SpendGross"
                   ,"UM_AllChan_Internet_Magazine_SpendGross"
                   ,"UM_AllChan_Internet_OOH_SpendGross"
                   ,"UM_AllChan_Internet_NewsPaper_SpendGross" 
                   ,"UM_AllChan_Internet_Cinema_SpendGross"
)

independent <- ds.prep.model[independent_n]

independent$UM_AllChan_Internet_Radio_SpendGross       <- adstock_it(independent$UM_AllChan_Internet_Radio_SpendGross     , 0.1, 0.9, 500)
independent$UM_AllChan_Internet_TV_SpendGross          <- adstock_it(independent$UM_AllChan_Internet_TV_SpendGross        , 0.1, 0.9, 500)
independent$UM_AllChan_Internet_TradePress_SpendGross  <- adstock_it(independent$UM_AllChan_Internet_TradePress_SpendGross, 0.1, 0.9, 100)
independent$UM_AllChan_Internet_Magazine_SpendGross    <- adstock_it(independent$UM_AllChan_Internet_Magazine_SpendGross  , 0.1, 0.9, 500)
independent$UM_AllChan_Internet_OOH_SpendGross         <- adstock_it(independent$UM_AllChan_Internet_OOH_SpendGross       , 0.1, 0.9, 500)
independent$UM_AllChan_Internet_NewsPaper_SpendGross   <- adstock_it(independent$UM_AllChan_Internet_NewsPaper_SpendGross , 0.1, 0.9, 500)
independent$UM_AllChan_Internet_Cinema_SpendGross      <- adstock_it(independent$UM_AllChan_Internet_Cinema_SpendGross    , 0.1, 0.9, 500)

library(stats)

# " -1" in the formula leads to a intercept of 0

first_linear_model <- lm(target ~ . - 1, data = independent)

#
# Multipl. with the Beta of traditional media spend
#


beta_second_model <- first_linear_model$coefficients 
View(beta_second_model)
pval_second_model <- summary(first_linear_model)$coefficients[,4]
View(pval_second_model)
#
# These betas fit


