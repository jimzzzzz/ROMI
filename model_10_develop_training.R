# Model 10 - Telesales 3P

model <- "Model_10"

# Define target ------------------------------------------

target <- ds.prep$UM_TeleinSiS_3P_Sales_Units_OE

# Run adstocks ------------------------------------------------------------

source("adstock_all.R")

# These variables were in the original model

nm <- c(
  "UM_AllChan_3P_ListPrice_Dummy"
  ,"UM_AllChan_3P_LosWochos_PriceReduction"
  ,"UM_AllChan_AllProd_NationalorFederalHolidays_GeoWeightedDays"
  ,"Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V1"
  ,"UM_AllChan_3P_VP5_Stepchange"
  
)

ns <- c(
  "UM_AllChan_AllProd_PaidSearch_sum_Impressions_ProductTotal"
  ,"UM_AllChan_AllProd_NonProgTotal_Impressions"
  ,"UM_AllChan_AllProd_SocialFBInsta_Impressions"
  ,"UM_AllChan_HSIn_Radio_GRP"
  ,"UM_AllChan_HSIn_TV_GRP"
  ,"UM_AllChan_HSIn_Magazines_SpendGross"
  ,"UM_AllChan_HSIn_OOH_SpendGross"
  ,"UM_AllChan_3p_Whitemail_Number"
  ,"UM_AllChan_3p_Outbound_ReachedContacts"
  
)

ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

# adjust price ranking variable so that it goes through zero - subtract min value from every value
a <- ds.prep.model$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V1
min_a <- min(a)
ds.prep.model$Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V1 <- a - min_a


scaling_factor <- 1000
vLevel <- NULL
source("model code.R")

View(model_beta)
View(pVal)
View(INPUT)

