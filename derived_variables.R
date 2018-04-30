
ds.prep2 <- ds.prep

# Comp_AllChan_HSIn_Spend
#----------------------------------------------

temp <- ds.prep[, c("Comp_AllChan_HSIn_Internet_Spend",
                    "Comp_AllChan_HSIn_Mobile_Spend",
                    "Comp_AllChan_HSIn_Magazines_Spend",
                    "Comp_AllChan_HSIn_TV_Spend",
                    "Comp_AllChan_HSIn_PromoEvents_Spend",
                    "Comp_AllChan_HSIn_Radio_Spend",
                    "Comp_AllChan_HSIn_OOH_Spend",
                    "Comp_AllChan_HSIn_Instoremedia_Spend"
                    )]

ds.prep2$Comp_AllChan_HSIn_Spend <- rowSums(temp)

# Comp_AllChan_MOB_TV_GRP_T
#----------------------------------------------

temp <- ds.prep[,grep("Comp_AllChan_MOB_TV", colnames(ds.prep))]
colnames(temp)

# it is not clear from where the first value comes
ds.prep2$Comp_AllChan_MOB_TV_GRP_T <- shift(ds.prep$Comp_AllChan_MOB_TV_GRP)

# check
ds.prep2$Comp_AllChan_MOB_TV_GRP_T - shift(ds.prep$Comp_AllChan_MOB_TV_GRP)

# Comp_AllChan_TV_TV_GRP_T
#----------------------------------------------

temp <- ds.prep[,grep("Comp_AllChan_TV_TV_GR", colnames(ds.prep))]
colnames(temp)

# it has different lag periods for different parts of the data
# it is not clear how these lags are determined


# UM_AllChan_1P_ListPrice_Dummy
#----------------------------------------------

temp <- ds.prep[,grep("UM_AllChan_1P_ListP", colnames(ds.prep))]
colnames(temp)

# it is not clear what the cutoff is
ds.prep2$UM_AllChan_1P_ListPrice_Dummy <- ifelse(ds.prep2$UM_AllChan_1P_ListPrice_Euros > 40, 1, 0)

# check
sum(ds.prep2$UM_AllChan_1P_ListPrice_Dummy - ds.prep$UM_AllChan_1P_ListPrice_Dummy)
  
# UM_AllChan_2P_ListPrice_Dummy
#----------------------------------------------

temp <- ds.prep[,grep("UM_AllChan_2P_ListPrice", colnames(ds.prep)), drop = F]
colnames(temp)

# it is not clear what the cutoff is
ds.prep2$UM_AllChan_2P_ListPrice_Dummy <- ifelse(ds.prep2$UM_AllChan_2P_ListPrice_Euros > 40, 1, 0)

# check
sum(ds.prep2$UM_AllChan_2P_ListPrice_Dummy - ds.prep$UM_AllChan_2P_ListPrice_Dummy)

# UM_AllChan_2P_LosWochos_Stepchange_T
#----------------------------------------------

temp <- ds.prep[,grep("UM_AllChan_2P_LosWochos_Stepchange", colnames(ds.prep)), drop = F]
colnames(temp)

ds.prep2$UM_AllChan_2P_LosWochos_Stepchange_T <- shift(ds.prep$UM_AllChan_2P_LosWochos_Stepchange)

# check - there are some differences
ds.prep2$UM_AllChan_2P_LosWochos_Stepchange_T - ds.prep$UM_AllChan_2P_LosWochos_Stepchange

# UM_AllChan_2P_OOH_SpendGross_T
#----------------------------------------------

temp <- ds.prep[,grep("UM_AllChan_2P_OOH_SpendGross", colnames(ds.prep)), drop = F]
colnames(temp)

# lagged for some points for some not

# UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_T
#----------------------------------------------

temp <- ds.prep[,c("UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_T", "UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product"), drop = F]
colnames(temp)

# lagged for some points for some not


# UM_AllChan_2P_SocialFBInsta_Impressions_T
#----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_2P_Social", colnames(ds.prep)), drop = F]
colnames(temp)

temp <- ds.prep[,c("UM_AllChan_2P_SocialFB.Insta_Impressions", "UM_AllChan_2P_SocialFBInsta_Impressions_T")]

# only one value is lagged; not clear what the logic is
# from 53 on they look lagged

# UM_AllChan_2P_TradePress_SpendGross2
#----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_2P_TradePress_", colnames(ds.prep)), drop = F]
colnames(temp)

# not clear how it is derived

# UM_AllChan_2P_VP3_Stepchange2
#----------------------------------------------
# 
# temp <- ds.prep[, grep("UM_AllChan_2P_VP3_Stepch", colnames(ds.prep)), drop = F]
# colnames(temp)


# UM_AllChan_2p3p_Outbound_ReachedContacts_T
#----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_2p3p_Outboun", colnames(ds.prep)), drop = F]
colnames(temp)

# lagged from 53 onward

ds.prep2$UM_AllChan_2p3p_Outbound_ReachedContacts_T <- ifelse(1:nrow(ds.prep) >= 53, shift(ds.prep$UM_AllChan_2p3p_Outbound_ReachedContacts), ds.prep$UM_AllChan_2p3p_Outbound_ReachedContacts)
ds.prep2$UM_AllChan_2p3p_Outbound_ReachedContacts_T[53] <- 0 # not sure why this is zero
ds.prep2$UM_AllChan_2p3p_Outbound_ReachedContacts_T - ds.prep$UM_AllChan_2p3p_Outbound_ReachedContacts_T
View(ds.prep2[,colnames(temp)])

# there are still differences in some periods; not sure why

# UM_AllChan_3P_ListPrice_Dummy
#----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_3P_ListPrice", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)

# temp2 <- init_data[, grep("UM_AllChan_3P_ListPrice", colnames(init_data)), drop = F]
# colnames(temp2)
# temp2 <- cbind(temp2, init_data$WS_Date)

ds.prep2$UM_AllChan_3P_ListPrice_Dummy <- ifelse(ds.prep$UM_AllChan_3P_ListPrice_Euros > 50, 1, 0)

# check
ds.prep2$UM_AllChan_3P_ListPrice_Dummy - ds.prep$UM_AllChan_3P_ListPrice_Dummy

# UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays_T
#-----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays_T", colnames(ds.prep)), drop = F]
temp <- ds.prep[, grep("UM_AllChan_AllProd_Karnveal_", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)

#  not sure why dummy for holidays is lagged

# again for some periods is lagged for others not

# UM_AllChan_AllProd_Magazines_SpendGross_T
#-----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_AllProd_Magazines_SpendGr", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)


# UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays_T
#-----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_AllProd_MarApr_Easter_Holidays", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)

# same as UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays_T

# UM_AllChan_AllProd_New_Year_Peak_2017
#-----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_AllProd_New_Year_Peak_2017", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)

# guess it should be 0 for all new variables as it is only dummy for 2017

# UM_AllChan_AllProd_Newspapers_SpendGross_T
#-----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_AllProd_Newspapers_", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)

temp$UM_AllChan_AllProd_Newspapers_SpendGross_T - temp$UM_AllChan_AllProd_Newspapers_SpendGross

# not clear what is lagged or if it lagged at all


# UM_AllChan_AllProd_NonProgDisplay_Impressions_T
#-----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_AllProd_NonProgDisplay_Impress", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)

# 87 is the first variable that looks lagged; however there are zeros before it and cannot be sure

# UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T
#-----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_AllProd_School_Holidays_GeoWeight", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)

temp$UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T - temp$UM_AllChan_AllProd_School_Holidays_GeoWeightedDays

# UM_AllChan_HSIn_TV_GRP_T
#-----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_HSIn_TV_", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)


# UM_AllChan_MSI_PostDiscount15_Dummy_T
#-----------------------------------------------

temp <- ds.prep[, grep("UM_AllChan_MSI_PostDiscount", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)

sum(temp$UM_AllChan_MSI_PostDiscount15_Dummy_T)

# only one lagged variable


# UM_D2DTeleout_3P_HSIn_CPO3_Spend_T2
#-----------------------------------------------

temp <- ds.prep[, grep("UM_D2DTeleout_3P_HSIn_CPO3_Sp", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)

temp$UM_D2DTeleout_3P_HSIn_CPO3_Spend_T2 - temp$UM_D2DTeleout_3P_HSIn_CPO3_Spend_T

# it seems that one is rounded the other not and thats the only difference

# UM_D2DTeleout_BCS_CPO3_Spend_U2
#-----------------------------------------------

temp <- ds.prep[, grep("UM_D2DTeleout_BCS_CPO3_", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)

# again seems that rounding is the difference

# UM_Digitaldir_2P_TP134_EuroValue_T
#-----------------------------------------------

temp <- ds.prep[, grep("UM_Digitaldir_2P_TP134_EuroV", colnames(ds.prep)), drop = F]
colnames(temp)
temp <- cbind(temp, ds.prep$WS_Date)


ds.prep2$UM_Digitaldir_2P_TP134_EuroValue_T <- shift(ds.prep$UM_Digitaldir_2P_TP134_EuroValue)

ds.prep2$UM_Digitaldir_2P_TP134_EuroValue_T - ds.prep$UM_Digitaldir_2P_TP134_EuroValue_T
ds.prep2$UM_Digitaldir_2P_TP134_EuroValue_T[1] <- 0

# this is lagged; however not sure for what period and if the lagging can change



