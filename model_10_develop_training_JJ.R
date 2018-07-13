# Model 10 - Telesales 3P

model <- "Model_10"

# Define target ------------------------------------------

target <- ds.prep$UM_TeleinSiS_3P_Sales_Units_OE

# Run adstocks ------------------------------------------------------------

source("adstock_all.R")

# These variables were in the original model

nm <- c(
  "UM_AllChan_3P_ListPrice_Dummy"
#  ,"UM_AllChan_3P_LosWochos_PriceReduction"
  ,"UM_AllChan_AllProd_NationalorFederalHolidays_GeoWeightedDays"
  ,"Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V1"
  ,"UM_AllChan_3P_VP5_Stepchange"
#  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays" # not significant
#  , "UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays_T" # wrong sign
#  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T" # not significant
#  , "UM_AllChan_3P_VP2_Stepchange" # took out because it is highly correlated with LP dummy
#  , "UM_AllChan_3P_VP4_Stepchange" # not significant so taken out
  , "UM_AllChan_3P_EffectivePriceRed_Euros" # wrong sign unless we take out LosWoch price reduction
#  , "UM_AllChan_3P_VP3_Stepchange" very high VIF

)

ns <- c(
#  "UM_AllChan_AllProd_PaidSearch_sum_Impressions_ProductTotal"
#  ,"UM_AllChan_AllProd_NonProgTotal_Impressions"
#  "UM_AllChan_AllProd_SocialFBInsta_Impressions"
#  ,"UM_AllChan_HSIn_Radio_GRP"
#  ,"UM_AllChan_HSIn_TV_GRP"
#  "UM_AllChan_HSIn_Magazines_SpendGross"
#  ,"UM_AllChan_HSIn_OOH_SpendGross"
#  "UM_AllChan_3p_Whitemail_Number"
#  "UM_AllChan_3p_Outbound_ReachedContacts" # wrong sign
#  ,"UM_AllChan_TV_Internet_SpendGross" wrong sign
#  ,"UM_AllChan_HSIn_TV_SpendGross_2L2" wrong sign
#  , "UM_AllChan_Total_All_SpendGross_2" #wrong sign
   "UM_AllChan_AllProd_HSIn_SpendGross_2"
#  , "Comp_AllChan_TV_TV_Spend" is significant bit using total media spend instead
  , "Comp_AllChan_Media_Spend"
#  , "UM_TeleinSiS_HSIn_OTCFreeBudget_RelNumOfSubsidy" # highly correlated with other variables
  , "UM_AllChan_3p_WhitemailXWOWI_Number" #replace 3P_whitemail_number
#  , "UM_AllChan_2p3p_Outbound_ReachedContacts_T" wrong sign
#  , "UM_AllChan_HSIn_SocialFBInsta_Impressions"
  , "UM_AllChan_TotSocialFBInsta_SocialFBInstaTot_Impressions" # this is the real total
#  , "UM_AllChan_HSIn_NonProgConnectedTV_Impressions" this is shit data - only first few weeks have values
#  , "UM_AllChan_AllProd_ProgStandard_Impressions" wrong sign
#  , "UM_AllChan_AllProd_ProgTot_PaidSearch_NonBrand_Product" wrong sign
#  , "UM_AllChan_AllProd_SocialYoutube_Impressions" wrong sign
  
)

ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

# adjust price ranking variable so that it goes through zero - subtract min value from every value
a <- ds.prep.model$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V1
min_a <- min(a)
ds.prep.model$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V1 <- a - min_a


scaling_factor <- 100 #changed this to 100 to avoid the warning message
vLevel <- NULL
source("model code.R")

View(model_beta)
View(pVal)
View(INPUT)

