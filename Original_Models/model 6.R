

n<-c(  # Zielvariable
  
  "UM_Retailind_2P_Sales_Units_OE"
  
  ,"UM_AllChan_2P_ListPrice_Dummy"
  ,"UM_AllChan_2P_EffectivePriceRed_Euros"
  ,"UM_AllChan_AllProd_NationalHolidays_GeoWeightedDays"  
  ,"UM_AllChan_AllProd_HolidaysXmas_NY_GeoWeightedDays"
  ,"UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays_T"
  ,"UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays_T"
  ,"UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T"
  ,"UM_AllChan_AllProd_New_Year_Peak_2017"
  ,"UM_AllChan_AllProd_Christmas_Dip_2016"
  ,"UM_AllChan_AllProd_Peak_June_2015"
  ,"UM_AllChan_Internet_Radio_SpendGross"
  ,"UM_AllChan_Internet_TV_SpendGross" 
  ,"UM_AllChan_Internet_TradePress_SpendGross"
  ,"UM_AllChan_Internet_Magazine_SpendGross"
  ,"UM_AllChan_Internet_OOH_SpendGross"
  ,"UM_AllChan_Internet_NewsPaper_SpendGross"
  ,"UM_AllChan_Internet_Cinema_SpendGross"
  ,"UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product_T"
  ,"UM_Retailind_2P_HSIn_CPO3_Spend_GT_20000" 
  ,"UM_Retailind_2P_HSIn_CPO3_Spend_LTE_20000"
  ,"UM_AllChan_2P_VP3_Stepchange2"
  ,"UM_DigitalIndir_AllProd_SalesSupport_Spend_Total"
  ,"Comp_AllChan_MOB_TV_GRP_T"
  ,"Comp_AllChan_TV_TV_GRP_T"  
  
)

ds.prep.model<-ds.prep[,names(ds.prep) %in% n]


# Create traditional media spend aggregate

ds.prep.model$traditional_media <- ds.prep.model$UM_AllChan_Internet_Radio_SpendGross + ds.prep.model$UM_AllChan_Internet_TV_SpendGross + ds.prep.model$UM_AllChan_Internet_TradePress_SpendGross+ ds.prep.model$UM_AllChan_Internet_Magazine_SpendGross+ ds.prep.model$UM_AllChan_Internet_OOH_SpendGross+ ds.prep.model$UM_AllChan_Internet_NewsPaper_SpendGross+ ds.prep.model$UM_AllChan_Internet_Cinema_SpendGross   
traditional_media2 <- ds.prep.model$UM_AllChan_Internet_Radio_SpendGross + ds.prep.model$UM_AllChan_Internet_TV_SpendGross + ds.prep.model$UM_AllChan_Internet_TradePress_SpendGross+ ds.prep.model$UM_AllChan_Internet_Magazine_SpendGross+ ds.prep.model$UM_AllChan_Internet_OOH_SpendGross+ ds.prep.model$UM_AllChan_Internet_NewsPaper_SpendGross+ ds.prep.model$UM_AllChan_Internet_Cinema_SpendGross   

ds.prep.model$UM_AllChan_2P_VP3_Stepchange2 <- adstock_it(ds.prep.model$UM_AllChan_2P_VP3_Stepchange2, 0.1, 0.01, max(ds.prep.model$UM_AllChan_2P_VP3_Stepchange2))
ds.prep.model$Comp_AllChan_MOB_TV_GRP_T       <- adstock_it(ds.prep.model$Comp_AllChan_MOB_TV_GRP, 0.1, 0.5, max(ds.prep.model$Comp_AllChan_MOB_TV_GRP_T))
ds.prep.model$Comp_AllChan_TV_TV_GRP_T        <- adstock_it(ds.prep.model$Comp_AllChan_TV_TV_GRP, 0.1, 0.5, max(ds.prep.model$Comp_AllChan_TV_TV_GRP_T))
ds.prep.model$UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product_T        <- adstock_it(ds.prep.model$UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product_T, 0.1, 0.9, 500)

ds.prep.model$traditional_media        <- adstock_it(ds.prep.model$traditional_media, 0.1, 0.1, 500)

# Fitting code ----
scaling_factor <- 100
complete.db <- ds.prep.model
# remove detailed media spend variables 
complete.db$UM_AllChan_Internet_Radio_SpendGross <- NULL
complete.db$UM_AllChan_Internet_TV_SpendGross <- NULL
complete.db$UM_AllChan_Internet_TradePress_SpendGross <- NULL
complete.db$UM_AllChan_Internet_Magazine_SpendGross <- NULL
complete.db$UM_AllChan_Internet_OOH_SpendGross <- NULL
complete.db$UM_AllChan_Internet_NewsPaper_SpendGross <- NULL
complete.db$UM_AllChan_Internet_Cinema_SpendGross <- NULL

dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
#vLevel <- NULL
fitOrder <- 1
vLevel <- -1.2

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

print(model_beta)
View(print(model_beta))

print(model_base)
View(print(model_base))



cov <- dlmSvd2var(kFilterSmooth$U.S, kFilterSmooth$D.S)[-1]
width <- t(qnorm(.95) * sqrt(sapply(cov,diag)))[1,1:dim(ind.Var)[2]]

se <- scaling_factor* width/(1.96)
zVal <- model_beta/se
pVal <- pnorm(-abs(zVal))
pVal <- c(0.0, pVal)
print(pVal)
View(pVal)



#
# Use the adstocked version for the target variable
#

target <- ds.prep.model$traditional_media

independent_n <- c("UM_AllChan_Internet_Radio_SpendGross"
  ,"UM_AllChan_Internet_TV_SpendGross"
  ,"UM_AllChan_Internet_TradePress_SpendGross"
  ,"UM_AllChan_Internet_Magazine_SpendGross"
  ,"UM_AllChan_Internet_OOH_SpendGross"
  ,"UM_AllChan_Internet_NewsPaper_SpendGross" 
  ,"UM_AllChan_Internet_Cinema_SpendGross"
)
independent <- ds.prep.model[independent_n]

independent$UM_AllChan_Internet_Radio_SpendGross       <- adstock_it(independent$UM_AllChan_Internet_Radio_SpendGross     , 0.1, 0.1, 500)
independent$UM_AllChan_Internet_TV_SpendGross          <- adstock_it(independent$UM_AllChan_Internet_TV_SpendGross        , 0.1, 0.1, 500)
independent$UM_AllChan_Internet_TradePress_SpendGross  <- adstock_it(independent$UM_AllChan_Internet_TradePress_SpendGross, 0.1, 0.1, 100)
independent$UM_AllChan_Internet_Magazine_SpendGross    <- adstock_it(independent$UM_AllChan_Internet_Magazine_SpendGross  , 0.1, 0.1, 500)
independent$UM_AllChan_Internet_OOH_SpendGross         <- adstock_it(independent$UM_AllChan_Internet_OOH_SpendGross       , 0.1, 0.1, 500)
independent$UM_AllChan_Internet_NewsPaper_SpendGross   <- adstock_it(independent$UM_AllChan_Internet_NewsPaper_SpendGross , 0.1, 0.1, 500)
independent$UM_AllChan_Internet_Cinema_SpendGross      <- adstock_it(independent$UM_AllChan_Internet_Cinema_SpendGross    , 0.1, 0.1, 500)

library(stats)

# " -1" in the formula leads to a intercept of 0

first_linear_model <- lm(target ~ . - 1, data = independent)

#
# Multipl. with the Beta of traditional media spend
#

first_linear_model$coefficients * 667.5775562964

beta_second_model <- first_linear_model$coefficients * as.numeric(model_beta[names(model_beta) == "traditional_media"])

#
# Cross Check
#

contri_target <- ds.prep.model$UM_Retailind_2P_Sales_Units_OE

contri_target / ind

beta_second_model <- first_linear_model$coefficients * as.numeric(model_beta[names(model_beta) == "traditional_media"])

as.numeric(beta_second_model[names(beta_second_model) == "UM_AllChan_Internet_TV_SpendGross"])       * independent$UM_AllChan_Internet_TV_SpendGross
as.numeric(beta_second_model[names(beta_second_model) == "UM_AllChan_Internet_Radio_SpendGross"])    * independent$UM_AllChan_Internet_Radio_SpendGross
as.numeric(beta_second_model[names(beta_second_model) == "UM_AllChan_Internet_Magazine_SpendGross"]) * independent$UM_AllChan_Internet_Magazine_SpendGross

