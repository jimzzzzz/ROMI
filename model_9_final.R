# Model 9 - D2D Teleout 3P

model <- "Model_9"

# Define target ------------------------------------------

target <- ds.prep$UM_D2DTeleout_3P_Sales_Units_OE

# Run adstocks ------------------------------------------------------------

source("adstock_all.R")

nm <- c(
  "UM_AllChan_3P_ListPrice_Dummy" #wrong sign
  , "UM_AllChan_3p_Outbound_ReachedContacts" # wrong sign
#  , "Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V1"
#  "Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V2"
#  , "UM_D2DTeleout_AllProd_SalesSupport_Spend_Total"
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_NationalorFederalHolidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  , "UM_D2DTeleout_3P_HSIn_CPO2_SpendPerOE_K2"
  , "UM_D2DTeleout_3P_HSIn_CPO3_Spend_T2"
#  , "UM_D2DTeleout_3P_HSIn_CPO1b_2_SpendPerOE_N" #wrong sign
#  , "UM_AllChan_3P_VP2_Stepchange"
#  , "UM_AllChan_3P_VP5_Stepchange"
#  , "UM_AllChan_3P_EffectivePriceRed_Euros" wrong sign
  , "UM_AllChan_3P_LosWochos_PriceReduction" # wrong sign
)

ns <- c(
#  "UM_AllChan_Non_B2B_sum_Impressions" wrong sign
  "UM_AllChan_HSIn_All_SpendGross_2L2"
  , "UM_AllChan_Brand_All_SpendGross_2L2"
#  , "UM_AllChan_HSIn_Radio_GRP" taken out cos effect should be in HSIn spend
#  , "UM_AllChan_HSIn_Magazines_SpendGross" wrong sign
#  , "Comp_AllChan_Media_Spend" #wrong sign
#  , "UM_AllChan_2p3p_Outbound_ReachedContacts_T" wrong sign
  , "UM_AllChan_3p_WhitemailXWOWI_Number"
  , "UM_AllChan_HSIn_NonProgDisplay_Impressions"
#  , "UM_AllChan_AllProd_PaidSearch_sum_Impressions_BrandTotal" wrong sign
)

ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

scaling_factor <- 100
vLevel <- NULL
source("model code.R")

View(model_beta)
View(pVal)
View(INPUT)

# check adstocks used

output_list$UM_AllChan_HSIn_All_SpendGross_2L2$adstock_params
output_list$UM_AllChan_Brand_All_SpendGross_2L2$adstock_params
output_list$UM_AllChan_3p_WhitemailXWOWI_Number$adstock_params
output_list$UM_AllChan_HSIn_NonProgDisplay_Impressions$adstock_params

# output the raw support

rs <- c(
  "UM_AllChan_3P_ListPrice_Dummy" 
  , "UM_AllChan_3p_Outbound_ReachedContacts"
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_NationalorFederalHolidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  , "UM_D2DTeleout_3P_HSIn_CPO2_SpendPerOE_K2"
  , "UM_D2DTeleout_3P_HSIn_CPO3_Spend_T2"
  , "UM_AllChan_3P_LosWochos_PriceReduction"
  , "UM_AllChan_HSIn_All_SpendGross_2L2"
  , "UM_AllChan_Brand_All_SpendGross_2L2"
  , "UM_AllChan_3p_WhitemailXWOWI_Number"
  , "UM_AllChan_HSIn_NonProgDisplay_Impressions", 
  "UM_AllChan_HSIn_TV_SpendGross_2L2",
  "UM_AllChan_HSIn_TradePress_SpendGross_2L2", 
  "UM_AllChan_HSIn_Magazines_SpendGross_2L2",
  "UM_AllChan_HSIn_OOH_SpendGross_2L2", 
  "UM_AllChan_HSIn_Newspapers_SpendGross_2L2", 
  "UM_AllChan_Brand_Radio_SpendGross_2L2", 
  "UM_AllChan_Brand_TV_SpendGross_2L2",
  "UM_AllChan_Brand_TradePress_SpendGross_2L2", 
  "UM_AllChan_Brand_Magazines_SpendGross_2L2",
  "UM_AllChan_Brand_OOH_SpendGross_2L2", 
  "UM_AllChan_Brand_Newspapers_SpendGross_2L2"
)

write.csv(t(ds.prep[,names(ds.prep) %in% rs]), file = "model_9_raw_support.csv")

