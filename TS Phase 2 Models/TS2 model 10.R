# Model 10	UM_TeleinSiS_3P_Sales_Units_OE	

ds.prep <- read.csv("./Data/Model_Database_20180518.csv")
target <- ds.prep$UM_TeleinSiS_3P_Sales_Units_OE

ds.prep$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrand_Product <- (
  ds.prep$UM_AllChan_AllProd_PaidSearch_sum_Impressions_NonBrandTotal+
    ds.prep$UM_AllChan_AllProd_PaidSearch_sum_Impressions_ProductTotal
)  

#library(DataCombine)

ds.prep$UM_AllChan_HSIn_Brand_Radio_SpendGross_2 <- (
   ds.prep$UM_AllChan_Brand_Radio_SpendGross_2 +	
   ds.prep$UM_AllChan_HSIn_Radio_SpendGross_2
)

ds.prep$UM_AllChan_HSIn_Brand_TV_SpendGross_2 <- (
  ds.prep$UM_AllChan_HSIn_TV_SpendGross_2 +
  ds.prep$UM_AllChan_Brand_TV_SpendGross_2
)

ds.prep <- slide(ds.prep, Var = "UM_AllChan_HSIn_Brand_Radio_SpendGross_2", NewVar = "UM_AllChan_HSIn_Brand_Radio_SpendGross_2L2",slideBy = -2)
ds.prep$UM_AllChan_HSIn_Brand_Radio_SpendGross_2L2[is.na(ds.prep$UM_AllChan_HSIn_Brand_Radio_SpendGross_2L2)] <- 0

ds.prep <- slide(ds.prep, Var = "UM_AllChan_HSIn_Brand_TV_SpendGross_2", NewVar = "UM_AllChan_HSIn_Brand_TV_SpendGross_2L2",slideBy = -2)
ds.prep$UM_AllChan_HSIn_Brand_TV_SpendGross_2L2[is.na(ds.prep$UM_AllChan_HSIn_Brand_TV_SpendGross_2L2)] <- 0

ds.prep <- slide(ds.prep, Var = "UM_AllChan_AllProd_ProgrammaticTot_Impressions", NewVar = "UM_AllChan_AllProd_ProgrammaticTot_Impressions_L1",slideBy = -1)
ds.prep$UM_AllChan_AllProd_ProgrammaticTot_Impressions_L1[is.na(ds.prep$UM_AllChan_AllProd_ProgrammaticTot_Impressions_L1)] <- 0


ds.prep$UM_AllChan_AllProd_HSIN_TV_SocialFBInsta_Impressions <- (
  ds.prep$UM_AllChan_AllProd_SocialFBInsta_Impressions +
  ds.prep$UM_AllChan_HSIn_SocialFBInsta_Impressions +
  ds.prep$UM_AllChan_TV_SocialFBInsta_Impressions
)

n<-c( 
  "UM_AllChan_3P_ListPrice_Dummy"
  ,"UM_AllChan_3P_EffectivePriceRed_Euros"
  ,"UM_AllChan_AllProd_NationalorFederalHolidays_GeoWeightedDays"
  ,"UM_AllChan_AllProd_PaidSearch_sum_Impressions_ProductTotal" #
  ,"UM_AllChan_AllProd_ProgrammaticTot_Impressions_L1"
  ,"UM_AllChan_AllProd_HSIN_TV_SocialFBInsta_Impressions" #
#  ,"UM_AllChan_HSIn_Brand_Radio_SpendGross_Clean" # explain
  ,"UM_AllChan_HSIn_Brand_Radio_SpendGross_2L2"
#  ,"UM_AllChan_HSIn_Brand_TV_SpendGross_Clean" # explain
  ,"UM_AllChan_HSIn_Brand_TV_SpendGross_2L2"
  ,"Comp_AllChan_Media_Spend"
  ,"UM_AllChan_3P_VP6_Stepchange" #
  ,"UM_AllChan_2p3p_Outbound_ReachedContacts"
  ,"UM_AllChan_3p_WhitemailXWOWI_Number"
)

ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% n])

# Adstock first regression ------------------------------------------------

ds.prep.model$UM_AllChan_AllProd_PaidSearch_sum_Impressions_ProductTotal <- adstock_it(ds.prep.model$UM_AllChan_AllProd_PaidSearch_sum_Impressions_ProductTotal, 0.1, 0.4, 500)
ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions_L1 <- adstock_it(ds.prep.model$UM_AllChan_AllProd_ProgrammaticTot_Impressions_L1, 0.4, 0.9, 1000)
ds.prep.model$UM_AllChan_AllProd_HSIN_TV_SocialFBInsta_Impressions <- adstock_it(ds.prep.model$UM_AllChan_AllProd_HSIN_TV_SocialFBInsta_Impressions, 0.9, 0.9, 500)
ds.prep.model$UM_AllChan_HSIn_Brand_Radio_SpendGross_2L2 <- adstock_it(ds.prep.model$UM_AllChan_HSIn_Brand_Radio_SpendGross_2L2, 0.1, 0.4, 300)
ds.prep.model$UM_AllChan_HSIn_Brand_TV_SpendGross_2L2 <- adstock_it(ds.prep.model$UM_AllChan_HSIn_Brand_TV_SpendGross_2L2, 0.1, 0.4, 300)
ds.prep.model$Comp_AllChan_Media_Spend <- adstock_it(ds.prep.model$Comp_AllChan_Media_Spend, 0.1, 0.1, 100)
ds.prep.model$UM_AllChan_2p3p_Outbound_ReachedContacts <- adstock_it(ds.prep.model$UM_AllChan_2p3p_Outbound_ReachedContacts, 0.99, 0.99, 100)
ds.prep.model$UM_AllChan_3p_WhitemailXWOWI_Number <- adstock_it(ds.prep.model$UM_AllChan_3p_WhitemailXWOWI_Number, 0.9, 0.9, 500)

#  Model code First regression---------------------------------------------

scaling_factor <- 1000
complete.db <- ds.prep.model
dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
vLevel <- -5.5
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

# Model betas First regression--------------------------------------------
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

# Successfully replicated