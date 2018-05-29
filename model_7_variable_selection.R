<<<<<<< HEAD
# Variable Categories - Not adstocked
# Seasonal, Price & Discount, Scarcity & Stepchange, Competition price, Commissions, Sales channel support

# Variable Categories - Adstocked
# GRPs, Spend, Comp GRP & spend, Promos, Direct marketing, Digital impressions

# Seasonal ----------------------------------------------------------------

# Create selection of variables according to category
sel <- datadict[which(datadict$Category=='Seasonal'),]

# Create a vector of variables names
Vsel <- sel[, "Variable_Name"]

# Select values from main data
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% Vsel])

# use Boruta function to determine significance of individual predictors
source("boruta.R")
# this returns a table of potentially significant variables. Sort it in order of importance

# Add chosen variables to vector of non-adstocked variables (nm) for testing in model
# Use model_7_develop.R

# Price and discount ------------------------------------------------------

sel <- datadict[which((datadict$Category=='Price' | datadict$Category=='VP') & (datadict$Product=='2P' | datadict$Product== 'AllProd')),]
Vsel <- sel[, "Variable_Name"]
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% Vsel])
source("boruta.R")

# List price dummy gives the wrong sign. Use List price instead

# test stepchange and scarcity indicators
# 

sel <- datadict[which(datadict$Category=='VP'),]
Vsel <- as.character(sel[, "Variable_Name"])
ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% Vsel])
source("boruta.R")

# For now keeping the original variables

# Move on to Competition variables - first non-adstock

sel <- datadict[which(datadict$Category=='Comp' & datadict$Activity =='Effectively_Mtl_Price' & datadict$Product=='2P'),]
Vsel <- sel[, "Variable_Name"]
ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% Vsel])
source("boruta.R")

# replace V6 with V3 which has better p-value

# Digital Impressions

sel <- datadict[which(datadict$Metric=='Impressions'),]
Vsel <- as.character(sel[, "Variable_Name"])
sel_adstock <- var_adstocked[,names(var_adstocked) %in% Vsel]
ds.prep.model <- cbind(target, sel_adstock)
source("boruta.R")

#  Tried "UM_AllChan_AllProd_NonProgSmartTV_sum_Impressions", "UM_AllChan_TV_SocialFBInsta_Impressions"
#  "UM_AllChan_AllProd_PaidSearch_Impressions_Brand" - significant but wrong sign

# GRPs

sel <- datadict[which(datadict$Category=='GRP'),]
Vsel <- as.character(sel[, "Variable_Name"])
sel_adstock <- var_adstocked[,names(var_adstocked) %in% Vsel]
ds.prep.model <- cbind(target, sel_adstock)
source("boruta.R")

# None of these really working. No GRP in original model so leave it
#  , "UM_AllChan_3P_Radio_GRP"
#  , "UM_AllChan_HSIn_TV_GRP_T"
#  , "UM_AllChan_Tel_TV_GRP" marginally significant
=======

# use datadict to select right variable

seasonal <- datadict[which(datadict$Category=='Seasonal'),]
# create a vector of seasonal variables
vSeasonal <- seasonal[, "Variable_Name"]

ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% vSeasonal])

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

nm <- c("UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T")

# next price and discount variables

price <- datadict[which((datadict$Category=='Price' | datadict$Category=='VP') & datadict$Product=='2P'),]
# create a vector of seasonal variables
vPrice <- price[, "Variable_Name"]

ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% vPrice])

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
corr_data <- ds.prep[,names(ds.prep) %in% vboruta]
res2<- rcorr(as.matrix(corr_data))
View(flattenCorrMatrix(res2$r, res2$P))
# define rule for including variables based on initial significance, then correlation
# ....later

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

complete.db <- ds.prep.model
dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
fitOrder <- 1

# Model definition happens over here
modelMle <- dlmMLE(y = dep.Var,
                   parm = rep(0.0, times = fitOrder + 1),
                   build = GetModel,
                   method = "CG")

# This is checking if we want to fix level variance based on prior information
if(!is.null(vLevel)){
  modelMle$par[2] <- vLevel
}

# Printing the variance for level determined
cat(sprintf("\t Model variances (Input) : %0.3f - par(%0.3f)\n", 
            exp(modelMle$par), modelMle$par))

# Model structure and parameter estimation happens here 
dataModel <- GetModel(modelMle$par)

# Kalman Filter to calculate the betas and Trend
kFilterData <- dlmFilter(dep.Var, dataModel)
kFilterSmooth <- dlmSmooth(kFilterData, dataModel)

# Extract model betas -----------------------------------------------------
# Extract betas and trend from the model
model_beta <- dropFirst(kFilterSmooth$s)[1, 1:dim(ind.Var)[2]] * scaling_factor
names(model_beta) <- colnames(ind.Var)

model_base <- kFilterSmooth$s[, dim(ind.Var)[2] + 1] * scaling_factor

View(model_beta)

# Calculate p-values ------------------------------------------------------

cov <- dlmSvd2var(kFilterSmooth$U.S, kFilterSmooth$D.S)[-1]
width <- t(qnorm(.95) * sqrt(sapply(cov,diag)))[1,1:dim(ind.Var)[2]]

se <- scaling_factor* width/(1.96)
zVal <- model_beta/se
pVal <- pnorm(-abs(zVal))
pVal <- c(0.0, pVal)

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

# run model code - separate R script
# V1 comes out as more significant but at slight loss of signficance of list price dummy. Keep V1, discard V6

# Move on to adstocking
# Start with GRPs

sel <- datadict[which(datadict$Category=='GRP'),]
Vsel <- as.character(GRP[, "Variable_Name"])

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

nm<-c(
  "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  , "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V1"
  #  , "UM_AllChan_3P_Radio_GRP"
  , "UM_AllChan_2P_Radio_GRP"
  #  , "UM_AllChan_2P_TV_GRP"
  
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm])

# radio 3P GRP not significant so try radio 2P
# radio 2P works. TV 2P beta is in wrong direction so drop it
# now we have
nm<-c(
  "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  , "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V1"
  , "UM_AllChan_2P_Radio_GRP"
)
>>>>>>> 16e83117326f6f19165d0863b023ddf30c37c7f2

# Spend

sel <- datadict[which(datadict$Category=='Spend'),]
Vsel <- as.character(sel[, "Variable_Name"])
<<<<<<< HEAD
sel_adstock <- var_adstocked[,names(var_adstocked) %in% Vsel]
ds.prep.model <- cbind(target, sel_adstock)
source("boruta.R")

# These variables are all individually significant although the p-values weaken when all used together
# "UM_AllChan_AllProd_Internet_SpendGross", "UM_AllChan_AllProd_TradePress_SpendGross", "UM_AllChan_AllProd_Magazines_SpendGross"
=======

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

# tried various spend variables but none work well in the model
# stay with this for now
nm<-c(
  "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays"
  , "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V1"
  , "UM_AllChan_2P_Radio_GRP"
  
)
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm])


>>>>>>> 16e83117326f6f19165d0863b023ddf30c37c7f2
