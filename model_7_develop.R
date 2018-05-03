# Model development code
# nm is list of non-adstocked
# ns is list of adstocked

# This is the original selection of non-adstocked variables which works well
nm <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_MSI_Discount15End_Dummy"
  , "UM_AllChan_MSI_PostDiscount15_Dummy"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6"
  , "UM_AllChan_2P_ScarcityIndicator_StepChange"
)

# Check if anything can be added
nm <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_MSI_Discount15End_Dummy"
  , "UM_AllChan_MSI_PostDiscount15_Dummy"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V3"
  , "UM_AllChan_2P_ScarcityIndicator_StepChange"
  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T"
  , "UM_AllChan_2P_LosWochos_Stepchange_T"
)

# So potentially can add school holidays and LosWochos variables

ns <- c(
   "UM_AllChan_AllProd_NonProgDisplay_Impressions_T"
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_Discount_Weighted"
  , "UM_AllChan_AllProd_Internet_SpendGross"
  , "UM_AllChan_AllProd_TradePress_SpendGross"
  , "UM_AllChan_AllProd_Magazines_SpendGross"
#  , "UM_AllChan_AllProd_OOH_SpendGross"
#  , "UM_AllChan_AllProd_Newspapers_SpendGross"
#  , "UM_AllChan_AllProd_Radio_SpendGross"
#  , "UM_AllChan_AllProd_InternetSocialIO_SpendGross"
#  , "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
)

# Use the following line if no adstocked variable included yet
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm])

# Use following line if non-adstocked and adstocked variables being used
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

# Run all chosen variables through model
scaling_factor <- 100
vLevel <- NULL
source("model code.R")



