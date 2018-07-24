# Model 3 - D2D Teleout 2P
model <- "Model_3"
# Define target ------------------------------------------

target <- ds.prep$UM_D2DTeleout_2P_Sales_Units_OE
# target <- as.integer(smoothed10) # idea was to see if using smoothed target would 
# enable commissions to be more important but this has not happened

# Run adstocks ------------------------------------------------------------

source("adstock_all.R")

peakweek <- read.csv("./Data/peakweek.csv") 
peakweek2 <- peakweek[,-3]

# Adding flags for the peak sales week and the week following increases Rsq from 52% to 78%.
# Adding a flag for the week before the peak week slightly reduces Rsq and reduces effectiveness of some other variables

# Model development code
# nm is list of non-adstocked
# ns is list of adstocked

nm <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_LosWochos_ScarcityIndicator"
#  , "UM_AllChan_AllProd_HolidaysXmas_NY_GeoWeightedDays"
#  , "UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays_T"
  , "UM_AllChan_2P_LosWochos_Stepchange_T"
  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
#  , "UM_AllChan_2P_VP4_Stepchange2"
#  , "UM_AllChan_2p3p_Outbound_ReachedContacts_T"
  , "UM_AllChan_2p_Outbound_ReachedContacts"
#  , "UM_AllChan_2p_Whitemail_Number" wrong sign and worsened a couple other variables
  , "UM_AllChan_2P_LosWochos_Stepchange_T"
#  , "UM_AllChan_2P_VP4_Stepchange2" completely not signififcant
#  , "UM_Digitaldir_2P_TP134_EuroValue_T" # why digital variable
#  , "UM_AllChan_2P_EffectivePriceRed_Euros" # not significant
#  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6" # not significant
#  , "Comp_AllChan_3P_Effectively_Mtl_Price_Avg_Ranking_V4" # not significant
  , "UM_D2DTeleout_2P_HSIn_CPO1b_SpendPerOE_C"
  , "UM_D2DTeleout_2P_HSIn_CPO3_Spend_S" # eventually wrong sign
#  , "UM_D2DTeleout_2P_HSIn_CPO1a_SpendPerOE_A" # wrong sign
#  , "UM_D2DTeleout_2P_HSIn_CPO1b_2_SpendPerOE_M" # wrong sign
#  , "UM_D2DTeleout_2P_HSIn_CPO2_SpendPerOE_J"
#  , "UM_D2DTeleout_AllProd_SalesSupport_Spend_Total" # wrong sign
)

ns <- c(
#  "UM_AllChan_HSIn_TV_GRP_T" #extremely strong
#  , "UM_AllChan_2P_Radio_GRP"
#  "UM_AllChan_HSIn_All_SpendGross_2"
  "UM_AllChan_HSIn_All_SpendGross_2L2"
#  , "UM_AllChan_2P_TradePress_SpendGross2"# wrong sign
#  , "UM_AllChan_2P_OOH_SpendGross_2"
#  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_T" # wrong sign
   , "Comp_AllChan_Media_Spend"
#  , "UM_AllChan_Brand_Internet_SpendGross_2L2" # wrong sign
#  , "UM_AllChan_2P_Radio_SpendGross_2"
#  , "UM_AllChan_1p2p_WhitemailXWOWI_Number" # wrong sign
#  , "UM_AllChan_1p2p_Whitemail_Number" # wrong sign
#  , "UM_AllChan_2p_Outbound_ReachedContacts" adstock version gives slightly better p value but worse rsq 
#  , "UM_AllChan_2P_PaidSearch_Impressions_Product_Discount_Weighted" # wrong sign
  , "UM_AllChan_AllProd_NonProgDisplay_sum_Impressions"
#  , "UM_AllChan_2P_SocialFBInsta_Impressions_T"
)
# ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% nm])
ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

# checking if week of the month variables (they do)
ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns], peakweek)

scaling_factor <- 100
vLevel <- NULL
source("model code.R")

View(model_beta)
View(pVal)
View(INPUT)

# idea is if we can get better model using smoothed output
# alternative to using flags for peaks and troughs

np<-c("KW_Year.Wk", 
      "UM_D2DTeleout_3P_Sales_Units_OE", 
      "UM_D2DTeleout_2P_Sales_Units_OE",
      "UM_D2DTeleout_2P_HSIn_CPO3_Spend_S")

plot.db <- cbind(ds.prep[,names(ds.prep) %in% np], peakweek)

plot.db$index <- 1:nrow(plot.db)  # create index variable
loessMod10 <- loess(UM_D2DTeleout_2P_Sales_Units_OE ~ index, data=plot.db, span=0.10) # 10% smoothing span
loessMod25 <- loess(UM_D2DTeleout_2P_Sales_Units_OE ~ index, data=plot.db, span=0.25) # 25% smoothing span
loessMod50 <- loess(UM_D2DTeleout_2P_Sales_Units_OE ~ index, data=plot.db, span=0.50) # 50% smoothing span

# get smoothed output
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50)

# Plot it
plot(plot.db$UM_D2DTeleout_2P_Sales_Units_OE, x=plot.db$index, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Sales", col="grey")
lines(smoothed10, x=plot.db$index, col="blue")
lines(smoothed25, x=plot.db$index, col="red")
lines(smoothed50, x=plot.db$index, col="green")

# choosing the 10 smooth
# 

# check adstocks used

output_list$UM_AllChan_HSIn_TV_GRP_T$adstock_params
output_list$UM_AllChan_2P_OOH_SpendGross_2$adstock_params
output_list$Comp_AllChan_Media_Spend$adstock_params
output_list$UM_AllChan_2P_Radio_SpendGross_2$adstock_params
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
  , "UM_AllChan_HSIn_TV_GRP_T"
  , "UM_AllChan_2P_OOH_SpendGross_2"
  , "Comp_AllChan_Media_Spend"
  , "UM_AllChan_2P_Radio_SpendGross_2"
  , "UM_AllChan_AllProd_NonProgDisplay_sum_Impressions"
)

write.csv(t(ds.prep[,names(ds.prep) %in% rs]), file = "model_3_raw_support.csv")
