
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


