# Model development code
# nm is list of non-adstocked
# ns is list of adstocked

# This is the original selection of non-adstocked variables
nm <- c(
  "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_EffectivePriceRed_Euros"
  , "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6"
#  , "UM_DigitalIndir_AllProd_AvgCashBackPaid_Euros_2016n17" although this is significant, leaving it out helps the digital variables
#  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays" ditto
#  , "UM_AllChan_2P_ScarcityIndicator_StepChange" ditto
)

ns<-c(
  "UM_HSIn_AllProd_TraditionalMedia_SpendGross"
  , "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_T"
  , "UM_AllChan_Non_B2B_sum_Impressions"
  , "UM_AllChan_AllProd_NonProgDisplay_Impressions_T"
  , "UM_AllChan_2P_SocialFBInsta_Impressions_T"
  , "Comp_AllChan_Media_Spend"
#  , "UM_AllChan_2P_Outbound_ReachedContacts2"
  , "UM_AllChan_2p_Email_Number"
#  , "UM_AllChan_Tel_Radio_GRP" is significant but worsens significance of digital variables
)

# This selection works well. Only Outbound has the wrong sign. Replace it with email
# LVF of 100 seems more stable than 1000
# NonB2B_sum is not great. Test some extra variables


# Use the following line if no adstocked variable included yet
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm])

# Use following line if non-adstocked and adstocked variables being used
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked[,names(var_adstocked) %in% ns])

# Run all chosen variables through model
scaling_factor <- 100
vLevel <- NULL
source("model code.R")

dataModel <- GetModel(modelMle$par)

# Kalman Filter to calculate the betas and Trend
kFilterData <- dlmFilter(dep.Var, dataModel)
kFilterSmooth <- dlmSmooth(kFilterData, dataModel)

# Extract betas and trend from the model
model_beta <- dropFirst(kFilterSmooth$s)[1, 1:dim(ind.Var)[2]] * scaling_factor
names(model_beta) <- colnames(ind.Var)

model_base <- dropFirst(kFilterSmooth$s)[, dim(ind.Var)[2] + 1] * scaling_factor

# Pvalues calculation
cov <- dlmSvd2var(kFilterSmooth$U.S, kFilterSmooth$D.S)[-1]
width <- t(qnorm(.95) * sqrt(sapply(cov,diag)))[1,1:dim(ind.Var)[2]]

se <- scaling_factor* width/(1.96)
zVal <- model_beta/se
pVal <- pnorm(-abs(zVal))

# the betas multiplied by the values of the independent vars
# contributions <- sweep(ind.Var, 2, model_beta, "*") * scaling_factor
contributions <- sweep(ind.Var, 2, model_beta, "*")

# defining fitted values 
Fit <- c(NA, NA, apply(as.data.frame(INPUT)[3:dim(INPUT)[2]],2,sum)+model_base)

# defining Actuals 
Actuals <- c(NA, NA ,ds.prep.model$target)

# defining Residuals 
Residuals <- Actuals - Fit

# defining the input matrix for subsequent work
INPUT <- cbind(pVal, model_beta, t(contributions))
INPUT <- rbind(t(Residuals), t(Fit), t(Actuals), t(c(0, 1, model_base)), INPUT)
rownames(INPUT)[1:4] <- c("Residuals","Fit","Actuals","Moving_Base")
colnames(INPUT)[1:2] <- c("pVal", "model_beta")

View(INPUT)