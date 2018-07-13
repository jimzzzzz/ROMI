# model 4
# secondary models required for product and brand
# product

target <- var_adstocked$UM_AllChan_HSIn_All_SpendGross_2 * 420.98031235483

# need to adstock components with same scalar. Let's just use same adstock as used with aggregate
# this gives the parameters (0.1, 0.1, 100)
output_list$UM_AllChan_HSIn_All_SpendGross_2$adstock_params

var_adstocked$UM_AllChan_HSIn_Radio_SpendGross_2 <- adstock_it(ds.prep$UM_AllChan_HSIn_Radio_SpendGross_2, 0.1, 0.1, 100)
var_adstocked$UM_AllChan_HSIn_TV_SpendGross_2 <- adstock_it(ds.prep$UM_AllChan_HSIn_TV_SpendGross_2, 0.1, 0.1, 100)
var_adstocked$UM_AllChan_HSIn_TradePress_SpendGross_2 <- adstock_it(ds.prep$UM_AllChan_HSIn_TradePress_SpendGross_2, 0.1, 0.1, 100)
var_adstocked$UM_AllChan_HSIn_Magazines_SpendGross_2 <- adstock_it(ds.prep$UM_AllChan_HSIn_Magazines_SpendGross_2, 0.1, 0.1, 100)
var_adstocked$UM_AllChan_HSIn_OOH_SpendGross_2 <- adstock_it(ds.prep$UM_AllChan_HSIn_OOH_SpendGross_2, 0.1, 0.1, 100)
var_adstocked$UM_AllChan_HSIn_Newspapers_SpendGross_2 <- adstock_it(ds.prep$UM_AllChan_HSIn_Newspapers_SpendGross_2, 0.1, 0.1, 100)

independent_n <- c("UM_AllChan_HSIn_Radio_SpendGross_2", 
                   "UM_AllChan_HSIn_TV_SpendGross_2",
                   "UM_AllChan_HSIn_TradePress_SpendGross_2", 
                   "UM_AllChan_HSIn_Magazines_SpendGross_2",
                   "UM_AllChan_HSIn_OOH_SpendGross_2", 
                   "UM_AllChan_HSIn_Newspapers_SpendGross_2"
)
independent <- var_adstocked[independent_n]

library(stats)

# " -1" in the formula leads to a intercept of 0

linear_model <- lm(target ~ . - 1, data = independent)

View(linear_model$coefficients)

summary(linear_model)$r.squared

write.csv(t(var_adstocked[,names(var_adstocked) %in% independent_n]), file = "model_4_adstock.csv")

#*****************#
##### brand #######
#*****************#

target <- var_adstocked$UM_AllChan_Brand_All_SpendGross_2L2 * 271.80372652774

# need to adstock components with same scalar. Let's just use same adstock as used with aggregate
# this gives the parameters (0.3, 0.1, 1000)
output_list$UM_AllChan_Brand_All_SpendGross_2L2$adstock_params

var_adstocked$UM_AllChan_Brand_Radio_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_Brand_Radio_SpendGross_2L2, 0.3, 0.1, 1000)
var_adstocked$UM_AllChan_Brand_TV_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_Brand_TV_SpendGross_2L2, 0.3, 0.1, 1000)
var_adstocked$UM_AllChan_Brand_TradePress_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_Brand_TradePress_SpendGross_2L2, 0.3, 0.1, 1000)
var_adstocked$UM_AllChan_Brand_Magazines_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_Brand_Magazines_SpendGross_2L2, 0.3, 0.1, 1000)
var_adstocked$UM_AllChan_Brand_OOH_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_Brand_OOH_SpendGross_2L2, 0.3, 0.1, 1000)
var_adstocked$UM_AllChan_Brand_Newspapers_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_Brand_Newspapers_SpendGross_2L2, 0.3, 0.1, 1000)

independent_n <- c("UM_AllChan_Brand_Radio_SpendGross_2L2", 
                   "UM_AllChan_Brand_TV_SpendGross_2L2",
                   "UM_AllChan_Brand_TradePress_SpendGross_2L2", 
                   "UM_AllChan_Brand_Magazines_SpendGross_2L2",
                   "UM_AllChan_Brand_OOH_SpendGross_2L2", 
                   "UM_AllChan_Brand_Newspapers_SpendGross_2L2"
)
independent <- var_adstocked[independent_n]

library(stats)

# " -1" in the formula leads to a intercept of 0

linear_model <- lm(target ~ . - 1, data = independent)

View(linear_model$coefficients)

summary(linear_model)$r.squared

write.csv(t(var_adstocked[,names(var_adstocked) %in% independent_n]), file = "model_4_adstock.csv")
