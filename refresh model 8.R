# refresh model 8

options(scipen= 999, digits=8)
library(dlm)
library(car)
library(data.table)
library(ggplot2)
library(lattice)
library(knitr)
library(Boruta)
library(Hmisc)

# Read in data ------------------------------------------------------------

ds.prep <- read.csv("./Data/Model_Database_20180404_Until KW201752_Changed SEM Values.csv")

# Define target and independents ------------------------------------------
# start with seasonals - pricing - competitive marketing - operational/structural events

target <- ds.prep$UM_Digitalind_2P_Sales_Units_OE

plot(target, type="l", col="blue", xlab="weeks", ylab="OEs", main="Sales Trend")
lines(ds.prep$UM_Digitaldir_2P_Sales_Units_OE, type="l", col="red")
# use datadict to select right variable

datadict <- read.csv("./Data/datadict.csv") # [,c('Variable_Name', 'Category')]
seasonal <- datadict[which(datadict$Category=='Seasonal'),]
# create a vector of seasonal variables
vSeasonal <- seasonal[, "Variable_Name"]

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% vSeasonal])

# what are the important seasonal variables (none in original model)

# use Boruta function to determine significance of individual predictors
set.seed(123)
boruta.train <- Boruta(target~., data=ds.prep.model, doTrace = 2)
final.boruta <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(final.boruta)
class(boruta.df)
View(boruta.df)

# only UM_AllChan_AllProd_School_Holidays_GeoWeightedDays and UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T
# are significant and UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T is better so we put it in model

nm<-c("UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T")

# next price and discount variables

price <- datadict[which((datadict$Category=='Price' | datadict$Category=='VP') & datadict$Product=='2P'),]
# create a vector of seasonal variables
vPrice <- price[, "Variable_Name"]

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% vPrice])

# use Boruta function to determine significance of individual predictors
set.seed(123)
boruta.train <- Boruta(target~., data=ds.prep.model, doTrace = 2)
final.boruta <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(final.boruta)
class(boruta.df)
View(boruta.df)
# keep just confimed variables
boruta.df <- subset(attStats(final.boruta), decision == "Confirmed")
# do correlation matrix of all confirmed variables
vboruta <- row.names(boruta.df)

# for now we shall test UM_AllChan_2P_LosWochos_Stepchange_T, UM_AllChan_2P_ListPrice_Dummy, 
# UM_AllChan_2P_VP3_Stepchange, UM_AllChan_2P_EffectivePriceRed_Euros

# add in significant variables one at a time

nm<-c(
  "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
#  , "UM_AllChan_2P_LosWochos_Stepchange_T"
  , "UM_AllChan_2P_ListPrice_Dummy"
#  , "UM_AllChan_2P_VP3_Stepchange"
#  , "UM_AllChan_2P_VP4_Stepchange"
  , "UM_AllChan_2P_EffectivePriceRed_Euros")

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm])

scaling_factor <- 100
vLevel <- NULL
source("model code.R")
View(model_beta)
View(pVal)

# Just keep the school holidays, list price dummy and price reduction variables. 

nm<-c(
  "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  , "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros")

# Move on to Competition variables - first non-adstock

comp.price <- datadict[which(datadict$Category=='Comp' & datadict$Activity =='Effectively_Mtl_Price' & datadict$Product=='2P'),]
# create a vector of seasonal variables
vCompPrice <- comp.price[, "Variable_Name"]

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% vCompPrice])

# use Boruta function to determine significance of individual predictors
set.seed(123)
boruta.train <- Boruta(target~., data=ds.prep.model, doTrace = 2)
final.boruta <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(final.boruta)
class(boruta.df)
View(boruta.df)
# keep just confimed variables
boruta.df <- subset(attStats(final.boruta), decision == "Confirmed")

# All variables are significanct. Test V1 (most significant) and V6 (used in previous model)

nm<-c(
  "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  #  , "UM_AllChan_2P_LosWochos_Stepchange_T"
  , "UM_AllChan_2P_ListPrice_Dummy"
  #  , "UM_AllChan_2P_VP3_Stepchange"
  #  , "UM_AllChan_2P_VP4_Stepchange"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
#  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V1"
 , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6"
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm])

# V1 comes out as more significant but at slight loss of signficance of list price dummy. Keep V1, discard V6

# Move on to adstocking
# Start with GRPs

sel <- datadict[which(datadict$Category=='GRP'),]
Vsel <- as.character(sel[, "Variable_Name"])

# all data adstocked to start with, just select the GRP adstocks and run through boruta

sel_adstock <- var_adstocked[,names(var_adstocked) %in% Vsel]

# run boruta on adstocks

ds.prep.model <- cbind(target, sel_adstock)

set.seed(123)
boruta.train <- Boruta(target~., data=ds.prep.model, doTrace = 2)
final.boruta <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(final.boruta)
class(boruta.df)
View(boruta.df)

# choose best radio and best TV GRPs
# code was wrong because ds.prep has unadstocked data

nm<-c( #non-adstocked
  "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  , "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V1"
)

ns<-c( #adstocked
#  "UM_AllChan_3P_Radio_GRP"
#  , "UM_AllChan_2P_TV_GRP"
  "UM_AllChan_Tel_Radio_GRP"
#  , "UM_AllChan_3P_TV_GRP"
)
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

scaling_factor <- 100
vLevel <- NULL
source("model code.R")
View(model_beta)
View(pVal)


# UM_AllChan_Tel_Radio_GRP works well. 

nm<-c( #not adstocked
  "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  , "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V1"
)
ns<-c( #adstocked
  "UM_AllChan_Tel_Radio_GRP"
)

# Spend

sel <- datadict[which(datadict$Category=='Spend'),]
Vsel <- as.character(sel[, "Variable_Name"])

# all data adstocked to start with, just select the GRP adstocks and run through boruta

sel_adstock <- var_adstocked[,names(var_adstocked) %in% Vsel]

# run boruta on adstocks

ds.prep.model <- cbind(target, sel_adstock)

set.seed(123)
boruta.train <- Boruta(target~., data=ds.prep.model, doTrace = 2)
final.boruta <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(final.boruta)
class(boruta.df)
View(boruta.df)

# trad media was in original model but is now inignificant with wrong sign 
# UM_AllChan_TV_Internet_SpendGross, UM_AllChan_AllProd_Internet_SpendGross are significant but with wrong sign
# UM_AllChan_HSIn_TV_SpendGross highly significant with positive sign
ns<-c(
  "UM_AllChan_Tel_Radio_GRP"
#  , "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
#  , "UM_AllChan_TV_Internet_SpendGross"
#  , "UM_AllChan_HSIn_Newspapers_SpendGross"
#  , "UM_AllChan_AllProd_Newspapers_SpendGross_T"
#  , "UM_AllChan_AllProd_Internet_SpendGross"
#  , "UM_AllChan_AllProd_Newspapers_SpendGross"
  , "UM_AllChan_HSIn_TV_SpendGross"
)
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

scaling_factor <- 100
vLevel <- NULL
source("model code.R")
View(model_beta)
View(pVal)
# so now we have adstocked
ns<-c(
  "UM_AllChan_Tel_Radio_GRP"
  , "UM_AllChan_HSIn_TV_SpendGross"
)

# comp spend
sel <- datadict[which(datadict$Category=='Comp' & datadict$Metric=='Spend'),]
Vsel <- as.character(sel[, "Variable_Name"])

# all data adstocked to start with, just select the GRP adstocks and run through boruta

sel_adstock <- var_adstocked[,names(var_adstocked) %in% Vsel]

# run boruta on adstocks

ds.prep.model <- cbind(target, sel_adstock)
source("boruta.R")

# test Comp_AllChan_TV_PromoEvents_Spend - sig and bad sign
# Comp_AllChan_HSIn_PromoEvents_Spend - ditto
# Comp_AllChan_TEL_TV_Spend  - not significant. one more
# Comp_AllChan_TEL_Radio_Spend - also not significant. stop here
# but what about Comp_AllChan_Media_Spend just about significant and correct sign

ns<-c(
  "UM_AllChan_Tel_Radio_GRP"
  , "UM_AllChan_HSIn_TV_SpendGross"
  , "Comp_AllChan_Media_Spend"
)

# Promo

sel <- datadict[which(datadict$Category=='Promo'),]
Vsel <- as.character(sel[, "Variable_Name"])
sel_adstock <- var_adstocked[,names(var_adstocked) %in% Vsel]
# run boruta on adstocks
ds.prep.model <- cbind(target, sel_adstock)
source("boruta.R")

# UM_Retaildir_3P_POSPromotion_Spend totally useless (as hoped)
# UM_AllChan_2P_LosWochos_Stepchange - is just significant but pushes other variables out
# UM_Retaildir_2P_TP21nTP100_EuroValue - also totally useless (as expected)
# UM_DigitalIndir_AllProd_AvgCashBackPaid_Euros_2016n17 - try it unadstocked - significant and positive beta
# UM_DigitalIndir_AllProd_AvgCommPaid_Euros_2016n17 is significant but gives negative beta

# test stepchange and scarcity indicators
# 

sel <- datadict[which(datadict$Category=='VP'),]
Vsel <- as.character(sel[, "Variable_Name"])

ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% Vsel])
source("boruta.R")


nm<-c( #not adstocked
  "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  , "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V1"
  , "UM_DigitalIndir_AllProd_AvgCashBackPaid_Euros_2016n17"
  , "UM_AllChan_2P_ScarcityIndicator_StepChange"
  , "UM_AllChan_2P_VP3_Stepchange"
)
ns<-c(
  "UM_AllChan_Tel_Radio_GRP"
  , "UM_AllChan_HSIn_TV_SpendGross"
  , "Comp_AllChan_Media_Spend"
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

scaling_factor <- 100
vLevel <- NULL
source("model code.R")
View(model_beta)
View(pVal)

# DM
sel <- datadict[which(datadict$Category=='DM'),]

Vsel <- as.character(sel[, "Variable_Name"])
sel_adstock <- var_adstocked[,names(var_adstocked) %in% Vsel]
ds.prep.model <- cbind(target, sel_adstock)
source("boruta.R")

# UM_AllChan_2p3p_Outbound_ReachedContacts_T - no
# UM_AllChan_3p_Whitemail_Number - no
# UM_AllChan_2P_Outbound_ReachedContacts2 - used in original model, but sign wrong here
# UM_AllChan_2p_Email_Number - good, pushing Comp_AlChan_Media_Spend out of significance

ns<-c(
  "UM_AllChan_Tel_Radio_GRP"
  , "UM_AllChan_HSIn_TV_SpendGross"
  , "Comp_AllChan_Media_Spend"
  , "UM_AllChan_2p_Email_Number"
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

scaling_factor <- 100
vLevel <- NULL
source("model code.R")
View(model_beta)
View(pVal)

# Digital Impressions

sel <- datadict[which(datadict$Metric=='Impressions'),]
Vsel <- as.character(sel[, "Variable_Name"])
sel_adstock <- var_adstocked[,names(var_adstocked) %in% Vsel]
ds.prep.model <- cbind(target, sel_adstock)
source("boruta.R")

# UM_AllChan_AllProd_NonProgDisplay_Impressions_T - not sig
# UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_T - not sig
# UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_Discount_Weighted - not sig
# UM_AllChan_HSIn_ProgVideo_Impressions - not sig
# UM_AllChan_AllProd_PaidSearch_Impressions_Brand - negative sign
# UM_AllChan_AllProd_PaidSearch_sum_Impressions_BrandTotal
# UM_AllChan_TotSocialFBInsta_SocialFBInstaTot_Impressions - not significant
# UM_AllChan_AllProd_SocialYoutube_Impressions - negative sign, but significant
# UM_AllChan_HSIn_ProgStandard_Impressions - negative sign
# UM_AllChan_B2B_sum_Impressions - negative sign
# UM_AllChan_Non_B2B_sum_Impressions - negative sign

nm<-c( #not adstocked
  "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  , "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V1"
  , "UM_DigitalIndir_AllProd_AvgCashBackPaid_Euros_2016n17"
  , "UM_AllChan_2P_ScarcityIndicator_StepChange"
  , "UM_AllChan_2P_VP3_Stepchange"
)
ns<-c(
  "UM_AllChan_Tel_Radio_GRP"
  , "UM_AllChan_HSIn_TV_SpendGross"
  , "Comp_AllChan_Media_Spend"
  , "UM_AllChan_2p_Email_Number"
  , "UM_AllChan_2P_SocialFBInsta_Impressions_T"
#  , "Comp_AllChan_TV_PromoEvents_Spend"
)
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

scaling_factor <- 100
vLevel <- NULL
source("model code.R")
View(model_beta)
View(pVal)

# next time take this model and produce the output diagnostics
write.csv(ds.prep.model, file = "output.csv")

