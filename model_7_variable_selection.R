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

# Spend

sel <- datadict[which(datadict$Category=='Spend'),]
Vsel <- as.character(sel[, "Variable_Name"])
sel_adstock <- var_adstocked[,names(var_adstocked) %in% Vsel]
ds.prep.model <- cbind(target, sel_adstock)
source("boruta.R")

# These variables are all individually significant although the p-values weaken when all used together
# "UM_AllChan_AllProd_Internet_SpendGross", "UM_AllChan_AllProd_TradePress_SpendGross", "UM_AllChan_AllProd_Magazines_SpendGross"