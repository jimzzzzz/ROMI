# Generic variable selection
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
# Use model_X_develop.R

# Price and discount ------------------------------------------------------

### Change Product filter depending on '2P' or '3P' model
sel <- datadict[which((datadict$Category=='Price' | datadict$Category=='VP') & (datadict$Product=='2P' | datadict$Product== 'AllProd')),]
Vsel <- sel[, "Variable_Name"]
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% Vsel])
source("boruta.R")

# Stepchange and scarcity indicators
# 

sel <- datadict[which(datadict$Category=='VP'),]
Vsel <- as.character(sel[, "Variable_Name"])
ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% Vsel])
source("boruta.R")

# Competition price

### Change Product filter depending on '2P' or '3P' model
sel <- datadict[which(datadict$Category=='Comp' & datadict$Activity =='Effectively_Mtl_Price' & datadict$Product=='2P'),]
Vsel <- sel[, "Variable_Name"]
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% Vsel])
source("boruta.R")

# Commissions

### for Sales.Channel choose 'RetailIndir', 'Retaildir' or 'D2DTeleout' depending on model
sel <- datadict[which(datadict$Category=='CPO' & datadict$Sales.Channel=='Retailind'),]
Vsel <- sel[, "Variable_Name"]
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% Vsel])
source("boruta.R")

# Sales channel support

### Choose appropriate Sales Channel 'DigitalIndir', RetailIndir', 'Retaildir' or 'D2DTeleout' depending on model
sel <- datadict[which(datadict$Category=='Support' & datadict$Sales.Channel=='RetailIndir'),]
Vsel <- sel[, "Variable_Name"]
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% Vsel])
source("boruta.R")

# Adstock variable selection ----------------------------------------------

# GRPs

sel <- datadict[which(datadict$Category=='GRP'),]
Vsel <- as.character(sel[, "Variable_Name"])
# all data adstocked to start with, just select the GRP adstocks and run through boruta
ds.prep.model <- cbind(target, var_adstocked[,names(var_adstocked) %in% Vsel])
source("boruta.R")

# Spend

sel <- datadict[which(datadict$Category=='Spend'),]
Vsel <- as.character(sel[, "Variable_Name"])
ds.prep.model <- cbind(target, var_adstocked[,names(var_adstocked) %in% Vsel])
source("boruta.R")

# Competition spend

sel <- datadict[which(datadict$Category=='Comp' & datadict$Metric=='Spend'),]
Vsel <- as.character(sel[, "Variable_Name"])
ds.prep.model <- cbind(target, var_adstocked[,names(var_adstocked) %in% Vsel])
source("boruta.R")

# Competition GRPs

sel <- datadict[which(datadict$Category=='Comp' & datadict$Metric=='GRP'),]
Vsel <- as.character(sel[, "Variable_Name"])
ds.prep.model <- cbind(target, var_adstocked[,names(var_adstocked) %in% Vsel])
source("boruta.R")

# Promo

sel <- datadict[which(datadict$Category=='Promo'),]
Vsel <- as.character(sel[, "Variable_Name"])
ds.prep.model <- cbind(target, var_adstocked[,names(var_adstocked) %in% Vsel])
source("boruta.R")

# DM

sel <- datadict[which(datadict$Category=='DM'),]
Vsel <- as.character(sel[, "Variable_Name"])
ds.prep.model <- cbind(target, var_adstocked[,names(var_adstocked) %in% Vsel])
source("boruta.R")

# Digital Impressions

sel <- datadict[which(datadict$Metric=='Impressions'),]
Vsel <- as.character(sel[, "Variable_Name"])
ds.prep.model <- cbind(target, var_adstocked[,names(var_adstocked) %in% Vsel])
source("boruta.R")

