# Model 3 - D2D Teleout 2P
model <- "Model_3"
# Define target ------------------------------------------

target <- ds.prep$UM_D2DTeleout_2P_Sales_Units_OE

# Run adstocks ------------------------------------------------------------

source("adstock_all.R")

# Model development code
# nm is list of non-adstocked
# ns is list of adstocked

nm <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_LosWochos_ScarcityIndicator"
  , "UM_AllChan_AllProd_HolidaysXmas_NY_GeoWeightedDays"
  , "UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays_T"
  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T"
  , "UM_AllChan_2p3p_Outbound_ReachedContacts_T"
  , "UM_AllChan_2P_LosWochos_Stepchange_T"
#  , "UM_AllChan_2P_VP4_Stepchange2" completely not signififcant
#  , "UM_Digitaldir_2P_TP134_EuroValue_T" # why digital variable
#  , "UM_AllChan_2P_EffectivePriceRed_Euros" # not significant
#  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6" # not significant
#  , "UM_D2DTeleout_2P_HSIn_CPO3_Spend_S" # eventually wrong sign
#  , "UM_D2DTeleout_2P_HSIn_CPO1a_SpendPerOE_A" # wrong sign
#  , "UM_D2DTeleout_2P_HSIn_CPO1b_2_SpendPerOE_M" # wrong sign
#  , "UM_D2DTeleout_AllProd_SalesSupport_Spend_Total" # wrong sign
)

ns <- c(
  "UM_AllChan_HSIn_All_SpendGross_2L2"
#  , "UM_AllChan_Brand_All_SpendGross_2L2" wrong sign
#  "UM_AllChan_HSIn_TV_GRP_T" #extremely strong
#  , "UM_AllChan_2P_Radio_GRP"
#  "UM_AllChan_HSIn_All_SpendGross_2"
#  , "UM_AllChan_2P_TradePress_SpendGross2"# wrong sign
#  , "UM_AllChan_2P_OOH_SpendGross_2"
#  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_T" # wrong sign
  , "Comp_AllChan_Media_Spend"
#  , "UM_AllChan_Brand_Internet_SpendGross_2L2" # wrong sign
#  , "UM_AllChan_2P_Radio_SpendGross_2"
  , "UM_AllChan_2p3p_WhitemailXWOWI_Number" # wrong sign
#  , "UM_AllChan_1p2p_Whitemail_Number" # wrong sign
#  , "UM_AllChan_2P_PaidSearch_Impressions_Product_Discount_Weighted" # wrong sign
  , "UM_AllChan_AllProd_NonProgDisplay_sum_Impressions"
)
# ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% nm])
ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

scaling_factor <- 100
vLevel <- NULL
source("model code.R")

View(model_beta)
View(pVal)
View(INPUT)

# check adstocks used

output_list$UM_AllChan_HSIn_All_SpendGross_2L2$adstock_params
output_list$Comp_AllChan_Media_Spend$adstock_params
output_list$UM_AllChan_2p3p_WhitemailXWOWI_Number$adstock_params
output_list$UM_AllChan_AllProd_NonProgDisplay_sum_Impressions$adstock_params

# output the raw support

rs <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_LosWochos_ScarcityIndicator"
  , "UM_AllChan_AllProd_HolidaysXmas_NY_GeoWeightedDays"
  , "UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays_T"
  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T"
  , "UM_AllChan_2p3p_Outbound_ReachedContacts_T"
  , "UM_AllChan_2P_LosWochos_Stepchange_T"
  , "UM_AllChan_HSIn_All_SpendGross_2L2"
  , "Comp_AllChan_Media_Spend"
  , "UM_AllChan_2p3p_WhitemailXWOWI_Number"
  , "UM_AllChan_AllProd_NonProgDisplay_sum_Impressions", 
  "UM_AllChan_HSIn_Radio_SpendGross_2L2", 
  "UM_AllChan_HSIn_TV_SpendGross_2L2",
  "UM_AllChan_HSIn_TradePress_SpendGross_2L2", 
  "UM_AllChan_HSIn_Magazines_SpendGross_2L2",
  "UM_AllChan_HSIn_OOH_SpendGross_2L2", 
  "UM_AllChan_HSIn_Newspapers_SpendGross_2L2"
)

write.csv(t(ds.prep[,names(ds.prep) %in% rs]), file = "model_3_raw_support.csv")
