# Model 5 - Direct retail 2P

model <- "Model_5"

# Define target ------------------------------------------

target <- ds.prep$UM_Retaildir_2P_Sales_Units_OE

# Run adstocks ------------------------------------------------------------

source("adstock_all.R")

# Use model_variable_selection.R to select variables to enter here

# nm is list of non-adstocked


# ns is list of adstocked

nm <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "UM_AllChan_2P_LosWochos_Stepchange"
  , "UM_AllChan_2P_ScarcityIndicator_StepChange"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V3"
  , "Comp_AllChan_HSIn_TV_GRP"
  , "UM_AllChan_AllProd_NationalorFederalHolidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays_T"
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
  , "UM_Retaildir_2P_HSIn_CPO1b_2_SpendPerOE_M"
  , "UM_Retaildir_2P_HSIn_CPO3_Spend_S"
  , "UM_Retaildir_2P_TP21nTP100_EuroValue" # use original adstocks
  , "UM_AllChan_2P_VP5_StepchangeInitial" # use original adstocks
#  , "UM_AllChan_2P_VP4_Stepchange" # is significant but does not increase R2 and does increase VIFs
#  , "UM_AllChan_HSIn_RouterFreedom_Stepchange"
#  , "UM_Retaildir_2P_HSIn_CPO1a_SpendPerOE_A" # no other CPO variables work
#  , "UM_Retaildir_AllProd_SalesSupport_Spend_Deal_closer" # no sales support variables work
)

ns<-c(
  "UM_AllChan_HSIn_All_SpendGross_2L2"
#  , "UM_AllChan_Brand_All_SpendGross_2L2"
#  , "UM_AllChan_2p3p_Whitemail_Number"
#  , "UM_AllChan_1p2p_Whitemail_Number" # wrong sign
#  , "Comp_AllChan_HSIn_Instoremedia_Spend"
#  , "Comp_AllChan_HSIn_TV_GRP"
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product"
#  , "UM_AllChan_HSIn_NonProgDisplay_Impressions"
  , "UM_AllChan_2p3p_WhitemailXWOWI_Number"
#  , "UM_AllChan_2p_Email_Number"
  )


ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])
ds.prep.model$UM_Retaildir_2P_TP21nTP100_EuroValue <- adstock_it(ds.prep.model$UM_Retaildir_2P_TP21nTP100_EuroValue, 0.1, 0.99, max(ds.prep.model$UM_Retaildir_2P_TP21nTP100_EuroValue))
ds.prep.model$UM_AllChan_2P_VP5_StepchangeInitial <- adstock_it(ds.prep.model$UM_AllChan_2P_VP5_StepchangeInitial, 0.1, 0.1, max(ds.prep.model$UM_AllChan_2P_VP5_StepchangeInitial))

# adjust Sales support variable so that it goes through zero - subtract min value from every value
a <- ds.prep.model$Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V3
min_a <- min(a)
ds.prep.model$Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V3 <- a - min_a
a <- ds.prep.model$Comp_AllChan_HSIn_TV_GRP
min_a <- min(a)
ds.prep.model$Comp_AllChan_HSIn_TV_GRP <- a - min_a
# does it make sense to remove min of this CPO variable - waiting for response of Nitesh
a <- ds.prep.model$UM_Retaildir_2P_HSIn_CPO1b_2_SpendPerOE_M
min_a <- min(a)
ds.prep.model$UM_Retaildir_2P_HSIn_CPO1b_2_SpendPerOE_M <- a - min_a



# Run all chosen variables through model
scaling_factor <- 100
vLevel <- NULL
source("model code.R")

View(model_beta)
View(pVal)
View(INPUT)

# Check how CPO is correlated with other variables
cordata <- subset(ds.prep, select = -c(KW_Year.Wk))
View(cor(cordata$UM_Retaildir_2P_HSIn_CPO1b_2_SpendPerOE_M,cordata))

# check adstocks used

output_list$UM_AllChan_HSIn_All_SpendGross_2L2$adstock_params
output_list$UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product$adstock_params
output_list$UM_AllChan_2p3p_WhitemailXWOWI_Number$adstock_params

# output the raw support

rs <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "UM_AllChan_2P_LosWochos_Stepchange"
  , "UM_AllChan_2P_ScarcityIndicator_StepChange"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V3"
  , "Comp_AllChan_HSIn_TV_GRP"
  , "UM_AllChan_AllProd_NationalorFederalHolidays_GeoWeightedDays"
  , "UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays_T"
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
  , "UM_Retaildir_2P_HSIn_CPO1b_2_SpendPerOE_M"
  , "UM_Retaildir_2P_HSIn_CPO3_Spend_S"
  , "UM_Retaildir_2P_TP21nTP100_EuroValue" 
  , "UM_AllChan_2P_VP5_StepchangeInitial" 
  , "UM_AllChan_HSIn_All_SpendGross_2L2"
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product"
  , "UM_AllChan_2p3p_WhitemailXWOWI_Number"
  , "UM_AllChan_HSIn_Magazines_SpendGross_2L2"
  , "UM_AllChan_HSIn_Newspapers_SpendGross_2L2"
  , "UM_AllChan_HSIn_OOH_SpendGross_2L2"
  , "UM_AllChan_HSIn_Radio_SpendGross_2L2"
  , "UM_AllChan_HSIn_TradePress_SpendGross_2L2"
  , "UM_AllChan_HSIn_TV_SpendGross_2L2"
)

write.csv(t(ds.prep[,names(ds.prep) %in% rs]), file = "model_5_raw_support.csv")
