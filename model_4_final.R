# Model 4 - Inbound and SIS 2P

model <- "Model_4"

# Define target ------------------------------------------

target <- ds.prep$UM_TeleinSiS_2P_Sales_Units_OE

# Run adstocks ------------------------------------------------------------

source("adstock_all.R")

# Use model_variable_selection.R to select variables to enter here

# nm is list of non-adstocked
# ns is list of adstocked

# This is the original selection of non-adstocked variables
nm <- c(
  "UM_AllChan_2P_ListPrice_Dummy" # bad pval
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V5" # sign wrong
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
  , "UM_AllChan_2p_Whitemail_Number" #bad pval, why not use adstock?
  , "UM_AllChan_2P_LosWochos_Stepchange" # bad pval
  , "UM_AllChan_2P_VP5_Stepchange"
)

ns<-c(
  "UM_AllChan_HSIn_All_SpendGross_2"
#  "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product"
  , "UM_AllChan_AllProd_ProgrammaticTot_Impressions" # sign wrong
  , "UM_AllChan_AllProd_NonProgTotal_Impressions"
  , "Comp_AllChan_HSIn_Spend"  # sign wrong
  
)

# New variables selected --------------------------------------------------

# need to go through this from scratch

nm <- c(
  "UM_AllChan_2P_ListPrice_Dummy" # bad pval
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
#  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V5" # sign wrong
#  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays" # taken out to improve media spend
#  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays" # gives much better R2 than row above
  , "UM_AllChan_2p_Whitemail_Number" #bad pval, why not use adstock?
  , "UM_AllChan_2P_LosWochos_Stepchange" # bad pval
  , "UM_AllChan_2P_VP4_Stepchange"
#  , "UM_AllChan_HSIn_RouterFreedom_Stepchange"
)

ns<-c(
#  "UM_AllChan_2p3p_WhitemailXWOWI_Number"
#  "UM_AllChan_2p3p_Whitemail_Number"
#  "UM_AllChan_2p_Outbound_ReachedContacts"
#   "UM_AllChan_2p_Email_Number"
  "UM_AllChan_Brand_All_SpendGross_2L2"
  , "UM_AllChan_HSIn_All_SpendGross_2" # negative sign, also with lag
#  "UM_AllChan_2P_Radio_GRP" # wrong sign
#  , "Comp_AllChan_Media_Spend" # wrong sign
   , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product" 
#  , "UM_AllChan_AllProd_ProgrammaticTot_Impressions" # sign wrong
  , "UM_AllChan_AllProd_NonProgTotal_Impressions" # sign wrong
#  , "Comp_AllChan_HSIn_Spend" # sign wrong
#  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_Discount_Weighted" # wrong sign
#  , "UM_AllChan_AllProd_NonProgDisplay_Impressions_T"
#  , "UM_AllChan_All_ProgFB_Impressions" # wrong sign
#  , "UM_AllChan_TV_SocialFBInsta_Impressions" # wrong sign
)


# Use the following line if no adstocked variable included yet
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm])

# Use following line if non-adstocked and adstocked variables being used
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

# adjust Comp variable so that it goes through zero - subtract min value from every value
#a <- ds.prep.model$Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V5
#min_a <- min(a)
#ds.prep.model$Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V5 <- a - min_a

# Run all chosen variables through model
scaling_factor <- 100
vLevel <- NULL
source("model code.R")

View(model_beta)
View(pVal)
View(INPUT)

# check adstocks used

output_list$UM_AllChan_Brand_All_SpendGross_2L2$adstock_params
output_list$UM_AllChan_HSIn_All_SpendGross_2$adstock_params
output_list$UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product$adstock_params
output_list$UM_AllChan_AllProd_NonProgTotal_Impressions$adstock_params

# output the raw support

rs <- c(
  "UM_AllChan_2P_ListPrice_Dummy" 
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "UM_AllChan_2p_Whitemail_Number" 
  , "UM_AllChan_2P_LosWochos_Stepchange" 
  , "UM_AllChan_2P_VP4_Stepchange"
  , "UM_AllChan_Brand_All_SpendGross_2L2"
  , "UM_AllChan_HSIn_All_SpendGross_2"
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product" 
  , "UM_AllChan_AllProd_NonProgTotal_Impressions"
  , "UM_AllChan_Brand_Magazines_SpendGross_2L2"
  , "UM_AllChan_Brand_Newspapers_SpendGross_2L2"
  , "UM_AllChan_Brand_OOH_SpendGross_2L2"
  , "UM_AllChan_Brand_Radio_SpendGross_2L2"
  , "UM_AllChan_Brand_TradePress_SpendGross_2L2"
  , "UM_AllChan_Brand_TV_SpendGross_2L2"
  , "UM_AllChan_HSIn_Magazines_SpendGross_2"
  , "UM_AllChan_HSIn_Newspapers_SpendGross_2"
  , "UM_AllChan_HSIn_OOH_SpendGross_2"
  , "UM_AllChan_HSIn_Radio_SpendGross_2"
  , "UM_AllChan_HSIn_TradePress_SpendGross_2"
  , "UM_AllChan_HSIn_TV_SpendGross_2"
)

write.csv(t(ds.prep[,names(ds.prep) %in% rs]), file = "model_4_raw_support.csv")
