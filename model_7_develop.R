# Model 7 - Digital direct 2P

# Define target ------------------------------------------

target <- ds.prep$UM_Digitaldir_2P_Sales_Units_OE

# Run adstocks ------------------------------------------------------------

source("adstock_all.R")

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

ns <- c(
  "UM_AllChan_AllProd_NonProgDisplay_Impressions_T"
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_Discount_Weighted"
  , "UM_AllChan_HSIn_All_SpendGross_2"
  , "UM_AllChan_Non_B2B_sum_Impressions"
  , "UM_AllChan_2P_SocialFBInsta_Impressions_T"
  , "Comp_AllChan_Media_Spend"
)

# New variables selected --------------------------------------------------
nm <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_MSI_Discount15End_Dummy"
  , "UM_AllChan_MSI_PostDiscount15_Dummy"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6"
  , "UM_AllChan_2P_ScarcityIndicator_StepChange"
  , "UM_AllChan_2P_LosWochos_Stepchange_T"
)

ns <- c(
  "UM_AllChan_AllProd_NonProgDisplay_Impressions_T"
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_Discount_Weighted"
  , "UM_AllChan_HSIn_All_SpendGross_2"
  , "UM_AllChan_Non_B2B_sum_Impressions"
  , "UM_AllChan_2P_SocialFBInsta_Impressions_T"
  , "Comp_AllChan_Media_Spend"
  , "UM_AllChan_Brand_All_SpendGross_2"
)

# This selection works well. Only Outbound has the wrong sign. Replace it with email
# Scalar of 100 seems more stable than 1000
# NonB2B_sum is not great. Test some extra variables


# Use the following line if no adstocked variable included yet
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm])

# Use following line if non-adstocked and adstocked variables being used
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

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

source("contributions.R")

View(INPUT)