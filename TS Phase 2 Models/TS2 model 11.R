# Model 11	UM_Retaildir_3P_Sales_Units_OE	

ds.prep <- read.csv("./Data/Model_Database_20180518.csv")
target <- ds.prep$UM_Retaildir_3P_Sales_Units_OE

ds.prep$UM_AllChan_3P_All_SpendGross_2 <- (
  #    ds.prep$UM_AllChan_HSIn_Internet_SpendGross_2+
  ds.prep$UM_AllChan_3P_Magazines_SpendGross_2+
    ds.prep$UM_AllChan_3P_Newspapers_SpendGross_2+
    ds.prep$UM_AllChan_3P_OOH_SpendGross_2+
    ds.prep$UM_AllChan_3P_Radio_SpendGross_2+
    ds.prep$UM_AllChan_3P_TradePress_SpendGross_2+
    ds.prep$UM_AllChan_3P_TV_SpendGross_2+
    ds.prep$UM_AllChan_3P_Cinema_SpendGross_2
)

ds.prep$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product <- (
  ds.prep$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrandTotal+
    ds.prep$UM_AllChan_AllProd_PaidSearch_sum_Impressions_ProductTotal
)  

library(DataCombine)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_3P_All_SpendGross_2", NewVar = "UM_AllChan_3P_All_SpendGross_2L2",slideBy = -2)
ds.prep$UM_AllChan_3P_All_SpendGross_2L2[is.na(ds.prep$UM_AllChan_3P_All_SpendGross_2L2)] <- 0


# First regression --------------------------------------------------------

n<-c(  
  "UM_AllChan_3P_ListPrice_Dummy"
  ,"UM_AllChan_3P_EffectivePriceRed_Euros"
  ,"UM_AllChan_AllProd_NationalorFederalHolidays_GeoWeightedDays"
  ,"UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays"
  ,"UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
#  ,"UM_AllChan_Brand_Traditional_Media_SpendGross_Clean"
  ,"UM_AllChan_Brand_All_SpendGross_2L2"
#  ,"UM_AllChan_3P_Traditional_Media_SpendGross_Clean"
  ,"UM_AllChan_3P_All_SpendGross_2L2"
  ,"UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product" # calculated above
  ,"Comp_AllChan_Media_Spend"
  ,"Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V3"
  ,"UM_AllChan_3P_ScarcityIndicator_StepChange"
  ,"UM_AllChan_2p3p_WhitemailXWOWI_Number"
  ,"UM_AllChan_3P_VP5_StepchangeInitial"
  ,"UM_Retaildir_3P_HSIn_CPO1b_2_SpendPerOE_N"
  
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

# Adstock first regression ------------------------------------------------

ds.prep.model$UM_AllChan_3P_All_SpendGross_2L2 <- adstock_it(ds.prep.model$UM_AllChan_3P_All_SpendGross_2L2, 0.1, 0.4, 100)
ds.prep.model$UM_AllChan_Brand_All_SpendGross_2L2 <- adstock_it(ds.prep.model$UM_AllChan_Brand_All_SpendGross_2L2, 0.1, 0.1, 100)
ds.prep.model$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product <- adstock_it(ds.prep.model$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product, 0.9, 0.9, 100)
ds.prep.model$UM_AllChan_3P_VP5_StepchangeInitial <- adstock_it(ds.prep.model$UM_AllChan_3P_VP5_StepchangeInitial, 0.1, 0.01445, max(ds.prep.model$UM_AllChan_3P_VP5_StepchangeInitial))
ds.prep.model$Comp_AllChan_Media_Spend <- adstock_it(ds.prep.model$Comp_AllChan_Media_Spend, 0.1, 0.9, 500)
ds.prep.model$UM_AllChan_2p3p_WhitemailXWOWI_Number <- adstock_it(ds.prep.model$UM_AllChan_2p3p_WhitemailXWOWI_Number, 0.1, 0.9, max(ds.prep.model$UM_AllChan_2p3p_WhitemailXWOWI_Number))

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

# Successfully replicated