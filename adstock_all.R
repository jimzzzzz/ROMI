# adstock all the variables you want to adstock
# GRP, Spend, Promo, DM, Comp spend, digital, search
# not price, VP, scarcity, CPOs

var2ad <- datadict[which (  datadict$Category=='GRP'
                          | datadict$Category=='Spend'
                          | datadict$Category=='Promo'
                          | datadict$Category=='DM'
                          |(datadict$Category=='Comp' & datadict$Metric=='Spend')
                          |(datadict$Category=='Comp' & datadict$Metric=='GRP')
                          | datadict$Category=='Digital'
                          | datadict$Category=='Search'),]
Vvar2ad <- as.character(var2ad[, "Variable_Name"])
# select variables from main data to be adstocked
ad_vars <- ds.prep[,names(ds.prep) %in% Vvar2ad]

# remove any column with NAs
library(dplyr)
ad_vars <- ad_vars %>% select_if(~ !any(is.na(.)))

B <- expand.grid(learn = seq(0.1, 0.9, 0.2), decay = seq(0.1, 0.9, 0.2), scalar = c(100,500,1000))

var_adstocked <- ad_vars
output_list <- list()
for(i in Vvar2ad){
  print(i)
  output_list[[i]] <- best_adstock(ds.prep[, i], target, B)
  
  # or if you want to change it in the data
  var_adstocked[, i] <- output_list[[i]]$adstocked_vector_max
}

# how do we extract the adstock parameters

output_list$UM_AllChan_HSIn_All_SpendGross_2L2$adstock_params
