# model 4

n<-c(  # Target variable model 3
  
  "UM_TeleinSiS_2P_Sales_Units_OE"
  
  # Price & Discount
  
  , "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  
  # Media Spends
  
  , "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
  
  # Digital
  , "UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product"
  , "UM_AllChan_AllProd_ProgrammaticTot_Impressions"
  , "UM_AllChan_AllProd_NonProgTotal_Impressions"
  
  # Commissions
  
  # Sales channel support
  , "UM_AllChan_2p_Whitemail_Number"
  
  # Value proposition change
  , "UM_AllChan_2P_LosWochos_Stepchange"
  , "UM_AllChan_2P_VP5_Stepchange"
  
  # Competition
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V5"
  , "Comp_AllChan_HSIn_Spend"
  
  # GRP

  # Seasonality
  
  , "UM_AllChan_AllProd_MarApr_Easter_Holidays_GeoWeightedDays"
)

ds.prep.model <- ds.prep[,names(ds.prep) %in% n]

#
# Adstock the data
#

ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross <- adstock_it(ds.prep.model$UM_HSIn_AllProd_TraditionalMedia_SpendGross, 0.1, 0.6, 100)
ds.prep.model$UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product <- adstock_it(ds.prep.model$UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product, 0.1, 0.9, 500)
ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions, 0.6, 0.9, 500)
ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_NonProgTotal_Impressions, 0.1, 0.9, 500)
ds.prep.model$Comp_AllChan_HSIn_Spend <- adstock_it(ds.prep.model$Comp_AllChan_HSIn_Spend, 0.1, 0.9, 500)



scaling_factor <- 1000
complete.db <- ds.prep.model
dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
vLevel <- NULL
fitOrder <- 1

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

# Extract betas and trend from the model
model_beta <- dropFirst(kFilterSmooth$s)[1, 1:dim(ind.Var)[2]] * scaling_factor
names(model_beta) <- colnames(ind.Var)
model_base <- kFilterSmooth$s[, dim(ind.Var)[2] + 1] * scaling_factor
View(model_beta)
print(model_beta)

