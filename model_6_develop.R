# Model 6 - Indirect retail 2P

model <- "Model_6"

# Define target ------------------------------------------

target <- ds.prep$UM_Retailind_2P_Sales_Units_OE

# Run adstocks ------------------------------------------------------------

source("adstock_all.R")

# Use model_variable_selection.R to select variables to enter here

# nm is list of non-adstocked
# ns is list of adstocked

# This is the original selection of non-adstocked variables

nm <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "UM_AllChan_AllProd_NationalHolidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_HolidaysXmas_NY_GeoWeightedDays"
#  , "UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays_T"
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays_T"
  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T"
  , "UM_AllChan_AllProd_New_Year_Peak_2017"
  , "UM_AllChan_AllProd_Christmas_Dip_2016"
#  , "UM_AllChan_AllProd_Peak_June_2015"
  , "UM_Retailind_2P_HSIn_CPO3_Spend_GT_20000"
  , "UM_Retailind_2P_HSIn_CPO3_Spend_LTE_20000" # check other CPO such as 1b, 2
#  , "UM_Retailind_2P_HSIn_CPO1a_SpendPerOE_A" # wrong sign for this and other CPO1
#  , "UM_Retailind_2P_HSIn_CPO2_SpendPerOE_J" # wrong sign
#  , "UM_DigitalIndir_AllProd_SalesSupport_Spend_Total" # why digital indir?
  , "UM_RetailIndir_AllProd_SalesSupport_Spend_Total"
  , "UM_AllChan_2P_VP3_Stepchange2"
  , "UM_AllChan_2P_LosWochos_Stepchange_T"
#  , "UM_AllChan_2P_LosWochos_ScarcityIndicator" # significant but takes away from CPO3
#  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V4" # as above
  )

ns<-c(
#  "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
  "UM_AllChan_HSIn_All_SpendGross_2L2"
#  "UM_AllChan_Total_All_SpendGross_2L2" # less signigicant and reduces contribution
#  , "UM_AllChan_Brand_All_SpendGross_2L2" # wrong sign
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_T"
  , "Comp_AllChan_MOB_TV_GRP_T"
  , "Comp_AllChan_TV_TV_GRP_T"
#  , "Comp_AllChan_Media_Spend" # doesn't help
#  , "UM_AllChan_HSIn_NonProgMobile_Impressions"
)
  

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])
ds.prep.model$UM_AllChan_2P_VP3_Stepchange2 <- adstock_it(ds.prep.model$UM_AllChan_2P_VP3_Stepchange2, 0.1, 0.01, max(ds.prep.model$UM_AllChan_2P_VP3_Stepchange2))
# adjust Sales support variable so that it goes through zero - subtract min value from every value
#a <- ds.prep.model$UM_DigitalIndir_AllProd_SalesSupport_Spend_Total
#min_a <- min(a)
#ds.prep.model$UM_DigitalIndir_AllProd_SalesSupport_Spend_Total <- a - min_a

# Run all chosen variables through model
scaling_factor <- 100
vLevel <- -1.2
source("model code.R")

View(model_beta)
View(pVal)
View(INPUT)

# check adstocks used

output_list$UM_AllChan_HSIn_All_SpendGross_2L2$adstock_params
output_list$UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_T$adstock_params
output_list$Comp_AllChan_MOB_TV_GRP_T$adstock_params
output_list$Comp_AllChan_TV_TV_GRP_T$adstock_params
