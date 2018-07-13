# Model 13	UM_Digitaldir_3P_Sales_Units_OE	

ds.prep <- read.csv("./Data/Model_Database_20180518.csv")
target <- ds.prep$UM_Digitaldir_3P_Sales_Units_OE

ds.prep$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product <- (
  ds.prep$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrandTotal+
    ds.prep$UM_AllChan_AllProd_PaidSearch_sum_Impressions_ProductTotal
)  

ds.prep$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product_DiscWeighted <- ds.prep$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product * ds.prep$UM_AllChan_3P_EffectivePriceRed_Euros

View(ds.prep$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product_DiscWeighted)

# First regression --------------------------------------------------------

n<-c(  
  "UM_AllChan_3P_ListPrice_Dummy"
#  ,"UM_AllChan_HSIn_Traditional_Media_SpendGross_Clean"
  ,"UM_AllChan_HSIn_All_SpendGross_2L2"
#  ,"UM_AllChan_Brand_Traditional_Media_SpendGross_Clean"
 ,"UM_AllChan_Brand_All_SpendGross_2L2"
  ,"UM_AllChan_AllProd_ProgrammaticTot_Impressions"
  ,"UM_AllChan_AllProd_NonProgTotal_Impressions"
  ,"UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product_DiscWeighted" # explain
  ,"UM_AllChan_AllProd_SocialFBInsta_Impressions"
  ,"UM_AllChan_MSI_PostDiscount15_Dummy_T"
  ,"UM_AllChan_3P_ScarcityIndicator_StepChange"
  ,"UM_AllChan_3P_VP5_StepchangeInitial"
  ,"UM_AllChan_3P_VP6_Stepchange" # explain
  ,"Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4"
  ,"Comp_AllChan_Media_Spend"
  ,"UM_AllChan_3p_Outbound_ReachedContacts"
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

# Adstock first regression ------------------------------------------------

ds.prep.model$UM_AllChan_3P_VP5_StepchangeInitial <- adstock_it(ds.prep.model$UM_AllChan_3P_VP5_StepchangeInitial, 0.1, 0.01445, max(ds.prep.model$UM_AllChan_3P_VP5_StepchangeInitial))
ds.prep.model$UM_AllChan_HSIn_All_SpendGross_2L2 <- adstock_it(ds.prep.model$UM_AllChan_HSIn_All_SpendGross_2L2, 0.1, 0.55, 500)
ds.prep.model$UM_AllChan_Brand_All_SpendGross_2L2 <- adstock_it(ds.prep.model$UM_AllChan_Brand_All_SpendGross_2L2, 0.1, 0.8, 500)
ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions, 0.2, 0.9, 500)
ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions, 0.24, 0.39, 200)
ds.prep.model$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product_DiscWeighted <- adstock_it(ds.prep.model$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product_DiscWeighted, 0.9, 0.9, 500)
ds.prep.model$UM_AllChan_AllProd_SocialFBInsta_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_SocialFBInsta_Impressions, 0.1, 0.6, 100)
ds.prep.model$Comp_AllChan_Media_Spend <- adstock_it(ds.prep.model$Comp_AllChan_Media_Spend, 0.4, 0.7, 300)


#  Model code First regression---------------------------------------------

scaling_factor <- 100
complete.db <- ds.prep.model
dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
vLevel <- -0.5
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

# All these are too much off. The discount weighted variable is correct but need to check all the rest

UM_AllChan_3P_VP5_StepchangeInitial
162967.0086449637
UM_AllChan_3P_VP6_Stepchange # giving wrong sign
48.1859230513
UM_AllChan_3P_ScarcityIndicator_StepChange
222.2630750247
UM_AllChan_3p_Outbound_ReachedContacts
0.0070433086
Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4
-39.0080899820
UM_AllChan_AllProd_ProgrammaticTot_Impressions
55.4210867333
UM_AllChan_AllProd_NonProgTotal_Impressions
62.1319857202
UM_AllChan_AllProd_SocialFBInsta_Impressions
372.0349164896
Comp_AllChan_Media_Spend
-54.6778761651
UM_AllChan_3P_ListPrice_Dummy
-287.6288206105
UM_AllChan_MSI_PostDiscount15_Dummy_T
405.7405060868
UM_AllChan_Brand_All_SpendGross_2L2
241.0638568484
UM_AllChan_HSIn_All_SpendGross_2L2
104.2364091050
UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product_DiscWeighted
208.3392378299


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
