# Model 8 - Digital indirect 2P
model <- "Model_8"
# Define target ------------------------------------------

target <- ds.prep$UM_Digitalind_2P_Sales_Units_OE

# Run adstocks ------------------------------------------------------------

source("adstock_all.R")

# Original variables ------------------------------------------------------

# Model development code
# nm is list of non-adstocked
# ns is list of adstocked

# This is the original selection of non-adstocked variables
nm <- c( # all these variables ok for beta and p-value
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6"
)

ns <- c(
#  "UM_HSIn_AllProd_TraditionalMedia_SpendGross" # pval not great
  "UM_AllChan_HSIn_All_SpendGross_2"
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_T" # bad pval
  , "UM_AllChan_Non_B2B_sum_Impressions" # wrong sign
  , "UM_AllChan_AllProd_NonProgDisplay_Impressions_T" # bad pval
  , "UM_AllChan_2P_SocialFBInsta_Impressions_T" # ok
  , "Comp_AllChan_Media_Spend" # ok
  , "UM_AllChan_2P_Outbound_ReachedContacts2" # wrong sign, bad pval
)


# New variables selected --------------------------------------------------

nm <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
#  , "UM_AllChan_2P_EffectivePriceRed_Euros" # used to discount weight paid search
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6"
  , "UM_DigitalIndir_AllProd_AvgCashBackPaid_Euros_2016n17" 
  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays" 
  , "UM_AllChan_2P_ScarcityIndicator_StepChange" 
  , "UM_AllChan_2P_LosWochos_Stepchange"
)

ns <- c(
  "UM_AllChan_2P_SocialFBInsta_Impressions_T"
  , "Comp_AllChan_Media_Spend"
#  , "UM_AllChan_2P_Radio_GRP" 
#  , "UM_AllChan_Brand_All_SpendGross_2L2"
#  , "UM_AllChan_HSIn_All_SpendGross_2L2"
  , "UM_AllChan_Total_All_SpendGross_2L2"
  , "UM_AllChan_2p_Email_Number"
  , "UM_AllChan_2p3p_WhitemailXWOWI_Number" # not so significant but helps a couple of other variables
#  , "UM_AllChan_HSIn_ProgVideo_Impressions" # right sign, lacklustre pval, helps spend
#  , "UM_AllChan_2P_PaidSearch_Impressions_Product" # very significant, worsens pval of 4 other variables
  , "UM_AllChan_2P_PaidSearch_Impressions_Product_Discount_Weighted"
#  , "UM_AllChan_Non_B2B_sum_Impressions" # use or not?
#  , "UM_AllChan_AllProd_PaidSearch_sum_Impressions_BrandTotal" # very significant, very negative
#  , "UM_AllChan_HSIn_ProgStandard_Impressions" # negative 
#  , "UM_AllChan_2P_PaidSearch_Impressions_Brand"
)

# This selection works well. Only Outbound has the wrong sign. Replace it with email
# Scalar of 100 seems more stable than 1000
# NonB2B_sum is not great. Test some extra variables


# Use the following line if no adstocked variable included yet
ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% nm])

# Use following line if non-adstocked and adstocked variables being used
ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

# adjust Comp variable so that it goes through zero - subtract min value from every value
# this does not effect the betas produced but will reduce the contribution from this variable
# the lost contribution goes into the moving base, no other variable is effected
a <- ds.prep.model$Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6
min_a <- min(a)
ds.prep.model$Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6 <- a - min_a

# Run all chosen variables through model
scaling_factor <- 100
vLevel <- NULL
source("model code.R")

View(model_beta)
View(pVal)
View(INPUT)

# check adstocks used

output_list$UM_AllChan_2P_SocialFBInsta_Impressions_T$adstock_params
output_list$Comp_AllChan_Media_Spend$adstock_params
output_list$UM_AllChan_Total_All_SpendGross_2L2$adstock_params
output_list$UM_AllChan_2p_Email_Number$adstock_params
output_list$UM_AllChan_2p3p_WhitemailXWOWI_Number$adstock_params
output_list$UM_AllChan_2P_PaidSearch_Impressions_Product_Discount_Weighted$adstock_params

# output the raw support

rs <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6"
  , "UM_DigitalIndir_AllProd_AvgCashBackPaid_Euros_2016n17" 
  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays" 
  , "UM_AllChan_2P_ScarcityIndicator_StepChange" 
  , "UM_AllChan_2P_LosWochos_Stepchange"
  , "UM_AllChan_2P_SocialFBInsta_Impressions_T"
  , "Comp_AllChan_Media_Spend"
  , "UM_AllChan_2p_Email_Number"
  , "UM_AllChan_2p3p_WhitemailXWOWI_Number"
  , "UM_AllChan_2P_PaidSearch_Impressions_Product_Discount_Weighted"
  , "UM_AllChan_Total_All_SpendGross_2L2"
  , "UM_AllChan_HSIn_Magazines_SpendGross_2L2"
  , "UM_AllChan_HSIn_Newspapers_SpendGross_2L2"
  , "UM_AllChan_HSIn_OOH_SpendGross_2L2"
  , "UM_AllChan_HSIn_Radio_SpendGross_2L2"
  , "UM_AllChan_HSIn_TradePress_SpendGross_2L2"
  , "UM_AllChan_HSIn_TV_SpendGross_2L2"
  , "UM_AllChan_Brand_Magazines_SpendGross_2L2"
  , "UM_AllChan_Brand_Newspapers_SpendGross_2L2"
  , "UM_AllChan_Brand_OOH_SpendGross_2L2"
  , "UM_AllChan_Brand_Radio_SpendGross_2L2"
  , "UM_AllChan_Brand_TradePress_SpendGross_2L2"
  , "UM_AllChan_Brand_TV_SpendGross_2L2"
)

write.csv(t(ds.prep[,names(ds.prep) %in% rs]), file = "model_8_raw_support.csv")

