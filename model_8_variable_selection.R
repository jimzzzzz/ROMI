# Model 8 variable selection
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
# Use model_8_develop.R

# Price and discount ------------------------------------------------------

sel <- datadict[which((datadict$Category=='Price' | datadict$Category=='VP') & (datadict$Product=='2P' | datadict$Product== 'AllProd')),]
Vsel <- sel[, "Variable_Name"]
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% Vsel])
source("boruta.R")

# test stepchange and scarcity indicators
# 

sel <- datadict[which(datadict$Category=='VP'),]
Vsel <- as.character(sel[, "Variable_Name"])
ds.prep.model <- cbind(target, ds.prep[,names(ds.prep) %in% Vsel])
source("boruta.R")


# etc....add all the categories