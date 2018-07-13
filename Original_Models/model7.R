
library(car)
library(stringr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(dplyr)
library(gdata)
library(data.table)
library(lattice)
library(dlm)
library(car)
library(stats)



# Data for refresh
ds <- as.data.frame(read.csv2("Z:/DBM/07_Projects/36_ROMI/Technical/Database/Model_Database_2018-01-22_v1.csv", sep = ",", dec = ".", stringsAsFactors=FALSE))


# New data for validation of the model
ds <- as.data.frame(read.csv2("Z:/DBM/07_Projects/36_ROMI/Technical/Database/Model_Database_20180403_Until_KW201743_2.csv", sep = ",", dec = ".", stringsAsFactors=FALSE))

# Does not work properly for formatting character to date, to many values have NA
#ds$WS_Date_test <- as.Date(ds$WS_Date, "%d-%b-%y")

# gsub replaces , by .
# Only necessary for german csv-files
#ds <- as.data.frame(lapply(ds,function(x) if(is.character(x)|is.factor(x)) gsub(",",".",x) else x))

# Not executed, because after that are only NA in WS_Date
# ds <- as.data.frame(lapply(ds, function(x) as.numeric(as.character(x))))


ds.prep <- ds


# Variables with special characters
# UM_AllChan_2P_PaidSearch_Impressions_Non-Brand_Product (UM_AllChan_2P_PaidSearch_Impressions_Non_Brand_Product)
# UM_AllChan_2P_SocialFB&Insta_Impressions (UM_AllChan_2P_SocialFBInsta_Impressions / UM_AllChan_2P_SocialFBandInsta_Impressions)

#
# Drop CPO data
#
# At the beginning we had two databases, now we have only one, so we don't need this code any more
# select_st <- "CPO"
# throw_out <- which(!colnames(ds.prep) %in%  colnames(ds.prep[grepl( select_st , names( ds.prep ))  ] ))
# ds.prep <- ds.prep[, throw_out]



#
# End Drop CPO data
#


adstock_it <- function(vector, learn, decay, scalar){
  
  # Example
  #vector <- ds.prep$UM_AllChan_HSIn_Magazines_SpendGross
  #learn  <- 0.1
  #decay  <- 0.9
  #scalar <- 100
  
  n <- length(vector)
  output <- rep(0,n)
  
  vector <- (vector / max(vector)) * scalar 
  
  for(i in 1 : length(output)){
    
    Decay <- -decay
    HRF   <- learn
    
    if(i == 1) {zaehler <- 1} else {zaehler <- 1 - output[i-1] * exp(Decay)}
    nenner  <- exp(vector[i] / 100 * HRF )                                      # Teile durch 100 da es als Prozente angegeben werden soll! 
    
    output[i] <- 1 - (zaehler/nenner)
    
  }
  
  
  return(output)
}



#
# Adstock the data
#

# Linear Regression
ds.prep$UM_AllChan_Internet_Radio_SpendGross_AD7          <- adstock_it(ds.prep$UM_AllChan_Internet_Radio_SpendGross      , 0.1, 0.35, 500)
ds.prep$UM_AllChan_Internet_TV_SpendGross_AD7             <- adstock_it(ds.prep$UM_AllChan_Internet_TV_SpendGross         , 0.1, 0.35, 500)
ds.prep$UM_AllChan_Internet_TradePress_SpendGross_AD7     <- adstock_it(ds.prep$UM_AllChan_Internet_TradePress_SpendGross , 0.1, 0.35, 100)
ds.prep$UM_AllChan_Internet_Magazine_SpendGross_AD7       <- adstock_it(ds.prep$UM_AllChan_Internet_Magazine_SpendGross   , 0.1, 0.35, 500)
ds.prep$UM_AllChan_Internet_OOH_SpendGross_AD7            <- adstock_it(ds.prep$UM_AllChan_Internet_OOH_SpendGross        , 0.1, 0.35, 500)
ds.prep$UM_AllChan_Internet_NewsPaper_SpendGross_AD7      <- adstock_it(ds.prep$UM_AllChan_Internet_NewsPaper_SpendGross  , 0.1, 0.35, 500)
ds.prep$UM_AllChan_Internet_Cinema_SpendGross_AD7         <- adstock_it(ds.prep$UM_AllChan_Internet_Cinema_SpendGross     , 0.1, 0.35, 500)



# DLM
ds.prep$UM_AllChan_Non_B2B_sum_Impressions_AD7                                       <- adstock_it(ds.prep$UM_AllChan_Non_B2B_sum_Impressions                                       , 0.1, 0.99, 500)
ds.prep$UM_AllChan_AllProd_NonProgDisplay_Impressions_T_AD7                          <- adstock_it(ds.prep$UM_AllChan_AllProd_NonProgDisplay_Impressions_T                            , 0.2, 0.90, 500)
ds.prep$UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_Discount_Weighted_AD7  <- adstock_it(ds.prep$UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_Discount_Weighted , 0.1, 0.50, 800)
ds.prep$UM_AllChan_2P_SocialFBInsta_Impressions_T_AD7                                <- adstock_it(ds.prep$UM_AllChan_2P_SocialFBInsta_Impressions_T                              , 0.9, 0.99, 500)
# replication 
#ds.prep$Comp_AllChan_Media_Spend_AD7                                                 <- adstock_it(ds.prep$Comp_AllChan_Media_Spend                                                 , 0.1, 0.14, 500)

# validation
ds.prep$Comp_AllChan_Media_Spend_AD7                                                 <- adstock_it(ds.prep$Comp_AllChan_Media_Spend_T                                                , 0.1, 0.14, 500)

ds.prep$UM_HSIn_AllProd_TraditionalMedia_SpendGross_AD7                              <- adstock_it(ds.prep$UM_HSIn_AllProd_TraditionalMedia_SpendGross                              , 0.1, 0.08, 500)




n<-c(  # Zielvariable
  
  "UM_Digitaldir_2P_Sales_Units_OE"
  
  ,"UM_AllChan_2P_ListPrice_Dummy"
  ,"UM_AllChan_MSI_Discount15End_Dummy"
  ,"UM_AllChan_MSI_PostDiscount15_Dummy"
  
 # ,"UM_AllChan_Internet_Radio_SpendGross"
 #,"UM_AllChan_Internet_TV_SpendGross"
 #,"UM_AllChan_Internet_TradePress_SpendGross"
 #,"UM_AllChan_Internet_Magazine_SpendGross"
 #,"UM_AllChan_Internet_OOH_SpendGross"
 #,"UM_AllChan_Internet_NewsPaper_SpendGross"
 #,"UM_AllChan_Internet_Cinema_SpendGross"
  
  ,"UM_HSIn_AllProd_TraditionalMedia_SpendGross_AD7"
 
  ,"UM_AllChan_Non_B2B_sum_Impressions_AD7"
  ,"UM_AllChan_AllProd_NonProgDisplay_Impressions_T_AD7"
  ,"UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_Discount_Weighted_AD7"
  ,"UM_AllChan_2P_SocialFBInsta_Impressions_T_AD7"
  ,"Comp_AllChan_Media_Spend_AD7"
  ,"Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6"
  ,"UM_AllChan_2P_ScarcityIndicator_StepChange"
  
)



ds.prep.model<-ds.prep[,names(ds.prep) %in% n]

# Is every variable in our database?
sum(n %in% colnames(ds.prep)) == length(n)

dim(ds.prep.model)

n[!(n %in% colnames(ds.prep))]



# DLM


options(scipen= 999, digits=8)
library(dlm)
library(car)

GetModel <- function(parm){
  
  # Build the model
  modReg <- dlmModReg(ind.Var,
                      addInt = FALSE,
                      dV = 0,
                      dW = rep(0,  times = dim(ind.Var)[2]))
  modTrend <- dlmModPoly(fitOrder)
  dataModel <- modReg + modTrend
  diag(dataModel$V) <- c(exp(parm[1]))
  diag(dataModel$W) <- c(rep(0,
                             times = dim(ind.Var)[2]),
                         exp(parm[2:(fitOrder + 1)]))
  return(dataModel)
}



# Fitting code ----
scaling_factor <- 100


complete.db <- ds.prep.model

# Add confuse: Variables with less prediction power are "random"
#complete.db$confuse <- runif(nrow(complete.db)) *100

dep.Var <- complete.db[,1]/scaling_factor

#
# Which variables to use?
#


ind.Var <- complete.db[,2 : ncol(complete.db)]

dim(ind.Var)

vLevel <- -1

# This determines the number of unobserved components that we want to have in the model
#  In absence of not so significant seasonality component, we can fix level variance to 
# capture seasonal effects; This is in conjunction with dummy variables used for the same
fitOrder <- 1

#ind.Var <- as.data.table(sapply(ind.Var, function(x) x / ((max(abs(max(x)),abs(min(x)) ))/10)))


# Model definition happens over here
modelMle <- dlmMLE(y = dep.Var,
                   parm = rep(0.0, times = fitOrder + 1),
                   build = GetModel,
                   method = "CG")

# apply(ds.prep.model, MARGIN = 2, FUN = function(x) sum(is.na(x)))

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
View(print(model_beta))

print(model_base)
View(print(model_base))



cov <- dlmSvd2var(kFilterSmooth$U.S, kFilterSmooth$D.S)[-1]
width <- t(qnorm(.95) * sqrt(sapply(cov,diag)))[1,1:dim(ind.Var)[2]]

se <- scaling_factor* width/(1.96)
zVal <- model_beta/se
pVal <- pnorm(-abs(zVal))
#pVal <- c(0.0, pVal)
names(pVal) <- colnames(ind.Var)
print(pVal)
View(pVal)


# Compute the dependent variable for OLS - replication
ds.prep$UM_HSIn_AllProd_TraditionalMedia_SpendGross_Digitaldir_2P_Contributions <- ds.prep$UM_HSIn_AllProd_TraditionalMedia_SpendGross_AD7 * 97.944408 # pval 0.345360


# Compute the dependent variable for OLS - validation
ds.prep$UM_HSIn_AllProd_TraditionalMedia_SpendGross_Digitaldir_2P_Contributions <- ds.prep$UM_HSIn_AllProd_TraditionalMedia_SpendGross_AD7 * 22.64072  # pval 0.463109




##########################################################################

# OLS

target <- ds.prep$UM_HSIn_AllProd_TraditionalMedia_SpendGross_Digitaldir_2P_Contributions

independent_n <- c("UM_AllChan_Internet_Radio_SpendGross_AD7"
                   ,"UM_AllChan_Internet_TV_SpendGross_AD7"
                   ,"UM_AllChan_Internet_TradePress_SpendGross_AD7"
                   ,"UM_AllChan_Internet_Magazine_SpendGross_AD7"
                   ,"UM_AllChan_Internet_OOH_SpendGross_AD7"
                   ,"UM_AllChan_Internet_NewsPaper_SpendGross_AD7" 
                   ,"UM_AllChan_Internet_Cinema_SpendGross_AD7"
)
independent <- ds.prep[independent_n]


# " -1" in the formula leads to a intercept of 0
linear_model_ols <- lm(target ~ . - 1, data = independent)

model_beta_ols <- linear_model_ols$coefficients
View(print(model_beta_ols))

pval_ols <- summary(linear_model_ols)$coefficients[,4] 
View(print(pval_ols))


##########################################################################


#
# Progress the Data
#


coeff     <- kFilterSmooth$s[2 : 2, 1 : (ncol(kFilterSmooth$s) - 1)]
intercept <- kFilterSmooth$s[2 : nrow(kFilterSmooth$s), (ncol(kFilterSmooth$s)) ] 

dim(coeff)
length(intercept)

#A <- coeff * ind.Var   * scaling_factor
A <- sweep(ind.Var, MARGIN = 2, coeff, "*") * scaling_factor
intercept <- intercept * scaling_factor

y     <- dep.Var * scaling_factor
y_hat <- rowSums(A) + intercept




Overview <- as.data.table(cbind(coeff * scaling_factor, base = intercept, y_hat, y, resi = (y - y_hat)))


# resi2: To check whether the predicted and real values are time shifted, whether they are a little bit before or after each other
# We take the observation week and one week before and after and take the minimum of the three residuals
Overview$resi2 <- 0

for(i in 2 : (nrow(Overview) - 1) ){
  
  Overview$resi2[i] <- min( abs(Overview$y[i] - Overview$y_hat[i])
                            , abs(Overview$y[i] - Overview$y_hat[i-1])
                            , abs(Overview$y[i] - Overview$y_hat[i+1]))
  
}

#View(Overview)


#
# Residuals
#

qqnorm(Overview$resi, ylim = c(-200, 200))

summary(Overview$resi)


plot(Overview$resi, ylim = c(-200, 200))

plot(Overview$y, Overview$resi, xlab = "Dependent Variable", ylab = "residual", main = "Residual vs Real", ylim = c(-200, 200))

plot(Overview$y_hat, Overview$resi, xlab = "Estimate", ylab = "residual", main = "Residual vs Estimate", ylim = c(-200, 200), xlim = c(0, 1500))



boxplot(Overview$resi)

#
# R2
#


R2 <- 1 - (sum((Overview$y-Overview$y_hat )^2)/sum((Overview$y-mean(Overview$y))^2))
R2


distance1 <- sum(abs(Overview$resi))
distance1

distance2 <- sum(Overview$resi2)
distance2

#
# Plot
#

nobs <- 135
plot.db <- data.frame(cbind(Overview, date =  seq(1,nobs,1)))


(ggplot(plot.db, aes(x=date, y=resi)) 
 + geom_line(aes(x=date, y=y_hat, color = "predicted", group = 1), colour="red") + geom_point(aes(x=date, y=y_hat),shape = 1,size = 2)
 + geom_line(aes(x=date, y=y    , color = "real value", group = 1), colour="blue" ) + geom_point(aes(x=date, y=y),shape = 0,size = 2)
 + geom_line(aes(x=date, y=(base) , color = "Base", group = 1), colour="black" )
 + geom_bar(stat="identity")
)


#######################################


rownames(A) <- NULL

colnames(A) <- colnames(ind.Var)



A <- cbind(A, base = Overview$base, date = plot.db$date)

A_hat <- melt(A, id = c("date"))

A_hat_minus <- A_hat[A_hat$value >= 0,]

A_hat_plus  <- A_hat[A_hat$value <  0,]

View(cbind(A, base = Overview$base, pred = Overview$y_hat, real = Overview$y))

A <- cbind(A, base = Overview$base, pred = Overview$y_hat, real = Overview$y)


(ggplot(A_hat, aes(x = date, y = value, fill = variable)) + geom_area(colour = "black", size = .2, alpha = 0.4) ) 



#
# Save the Analytic
#

ordner    = "Z:/DBM/07_Projects/36_ROMI/Code_Refresh/"
modelname = "Model7/"
prefix    = "model7_"

#
# Save the Resi Plot
#

pdf(paste(ordner,modelname,prefix,"resi.pdf" ,sep = ""))

plot(Overview$resi, ylim = c(-400, 800))

dev.off()



#
# Save the Resi vs Real Plot
#

pdf(paste(ordner,modelname,prefix,"resi_vs_real.pdf" ,sep = ""))

plot(Overview$y, Overview$resi, xlab = "Dependent Variable", ylab = "residual", main = "Residual vs Real", ylim = c(-400, 800))

dev.off()


#
# Save the Resi vs Estimate Plot
#

pdf( paste(ordner,modelname,prefix,"resi_vs_estimate.pdf" ,sep = ""))

plot(Overview$y_hat, Overview$resi, xlab = "Estimate", ylab = "residual", main = "Residual vs Estimate", ylim = c(-400, 800))

dev.off()


#
# Save the Model Plot
#

pdf( paste(ordner,modelname,prefix,"model.pdf" ,sep = ""))

(ggplot(plot.db, aes(x=date, y=resi)) 
 + geom_line(aes(x=date, y=y_hat, color = "predicted", group = 1), colour="red") + geom_point(aes(x=date, y=y_hat),shape = 1,size = 2)
 + geom_line(aes(x=date, y=y    , color = "real value", group = 1), colour="blue" ) + geom_point(aes(x=date, y=y),shape = 0,size = 2)
 + geom_line(aes(x=date, y=(base) , color = "Base", group = 1), colour="black" )
 + geom_bar(stat="identity")
)

dev.off()
# Warning message:
# Stacking not well defined when ymin != 0 
# Reason: + geom_bar(stat="identity") -> the geom_bar in the ggplot-Package cannot handle negative values



#
# Save the betas
#

# DLM betas and significance
write.csv2(model_beta, file = paste(ordner,modelname,prefix,"beta.csv",sep = "") )
write.csv2(pVal, file = paste(ordner,modelname,prefix,"pval.csv",sep = "") )


# OLS betas and significance
write.csv2(model_beta_ols, file = paste(ordner,modelname,prefix,"beta_ols.csv",sep = "") )
write.csv2(pval_ols, file = paste(ordner,modelname,prefix,"pval_ols.csv",sep = "") )


# Save the matrix A
write.csv2(A, file = paste(ordner,modelname,prefix,"A.csv",sep = "") )

# Save the matrix Overview
write.csv2(Overview, file = paste(ordner,modelname,prefix,"Overview.csv",sep = ""))


#### !!!! #####
# The predicted and real values in the decomp in the tab "WEEKLY DECOMP - GROUPED" are based only on the DLM.




