
#Making a base model - needs to be in the same data set
#need to fix

n <- c(  # Target variable model 3
  
  "UM_D2DTeleout_2P_Sales_Units_OE"
  
  # Price & Discount
  
  , "UM_AllChan_2P_ListPrice_Dummy"
  , "UM_AllChan_2P_LosWochos_ScarcityIndicator"

  # Media Spends
  
  , "UM_AllChan_2P_TradePress_SpendGross2"
  , "UM_AllChan_2P_OOH_SpendGross_T"	
  
  # Digital
  , "UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product_T"

  # Commissions

  # Sales channel support
  , "UM_AllChan_2p3p_Outbound_ReachedContacts_T"
  
  # Value proposition change
  , "UM_AllChan_2P_VP4_Stepchange2"
  , "UM_Digitaldir_2P_TP134_EuroValue_T"
  , "UM_AllChan_2P_LosWochos_Stepchange_T"
  
  # GRP
  , "UM_AllChan_HSIn_TV_GRP_T"
  
  # Seasonality
  
  , "UM_AllChan_AllProd_School_Holidays_GeoWeightedDays_T" 
  , "UM_AllChan_AllProd_Karnveal_Holidays_GeoWeightedDays_T"
  , "UM_AllChan_AllProd_HolidaysXmas_NY_GeoWeightedDays"
)

ds.prep.model<-ds.prep[,names(ds.prep) %in% n]

#
# Adstock the data
#

ds.prep.model$UM_AllChan_HSIn_TV_GRP_T <- adstock_it(ds.prep.model$UM_AllChan_HSIn_TV_GRP_T, 0.1, 0.35, max(ds.prep.model$UM_AllChan_HSIn_TV_GRP))
ds.prep.model$UM_AllChan_2P_TradePress_SpendGross2 <- adstock_it(ds.prep.model$UM_AllChan_2P_TradePress_SpendGross2, 0.1, 0.35, 500)
ds.prep.model$UM_AllChan_2P_OOH_SpendGross_T <- adstock_it(ds.prep.model$UM_AllChan_2P_OOH_SpendGross_T, 0.1, 0.35, 500)
ds.prep.model$UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product_T <- adstock_it(ds.prep.model$UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product_T, 0.1, 0.9, 500)

scaling_factor <- 100
complete.db <- ds.prep.model
dep.Var <- complete.db[,1]/scaling_factor
ind.Var <- complete.db[,2:dim(complete.db)[2]]
vLevel <- NULL
fitOrder <- 1

#ind.Var <- as.data.table(sapply(ind.Var, function(x) x / ((max(abs(max(x)),abs(min(x)) ))/10)))
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
print(model_beta)
View(model_beta)


#######################
## Progress the Data ##
#######################

# take the second row of the table for the coefficients
# old code: coeff     <- kFilterSmooth$s[2 : nrow(kFilterSmooth$s), 1 : (ncol(kFilterSmooth$s) - 1)]
coeff     <- kFilterSmooth$s[2 : 2, 1 : (ncol(kFilterSmooth$s) - 1)]
intercept <- kFilterSmooth$s[2 : nrow(kFilterSmooth$s), (ncol(kFilterSmooth$s)) ] 

dim(coeff)
length(intercept)

# old code: A <- coeff * ind.Var   * scaling_factor
A <- sweep(ind.Var, MARGIN = 2, coeff, "*") * scaling_factor
intercept <- intercept * scaling_factor

y     <- dep.Var * scaling_factor
y_hat <- rowSums(A) + intercept

Overview <- as.data.table(cbind(coeff * 100, base = intercept * 100, y_hat, y, resi = (y - y_hat)))
# View(Overview)

# Residuals

qqnorm(Overview$resi)

summary(Overview$resi)

plot(Overview$resi)

plot(Overview$y, Overview$resi, xlab = "Dependent Variable", ylab = "residual", main = "Residual vs Real")

plot(Overview$y_hat, Overview$resi, xlab = "Estimate", ylab = "residual", main = "Residual vs Estimate")

boxplot(Overview$resi)

# R2

R2 <- 1 - (sum((Overview$y-Overview$y_hat )^2)/sum((Overview$y-mean(Overview$y))^2))
R2

# Plot

plot.db <- data.frame(cbind(Overview, date =  seq(1,nrow(complete.db),1)))

(ggplot(plot.db, aes(x=date, y=resi)) 
  + geom_line(aes(x=date, y=y_hat, color = "predicted", group = 1), colour="red") 
  + geom_line(aes(x=date, y=y    , color = "real value", group = 1), colour="blue" ) 
  + geom_line(aes(x=date, y=(base / 100) , color = "Base", group = 1), colour="black" )
  + geom_bar(stat="identity")
  + scale_color_discrete(name="Legend") )


