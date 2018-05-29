
# Set libraries -----------------------------------------------------------


options(scipen=999, digits=8)

library(dlm)
library(car)
library(data.table)
library(ggplot2)
# library(lattice)
# library(knitr)
library(stringr)
library(reshape2)
library(tidyr)
library(dplyr)
library(Boruta)
library(Hmisc)
# library(gdata)
library(openxlsx)

#-----------------------------------------------------------------------------------------------

# functions that would be used throughout the whole project are sourced here
source("helper_functions.R")
source("DLMModel.R")

# Read in data ------------------------------------------------------------

ds.prep <- read.csv("./Data/Model_Database_20180518.csv")
datadict <- read.csv("./Data/datadict_v2.csv") 

# Data transformations - All new variables must also be added to datadict.csv

ds.prep$UM_AllChan_Brand_All_SpendGross_2 <- (
#    ds.prep$UM_AllChan_Brand_Internet_SpendGross_2+
    ds.prep$UM_AllChan_Brand_Magazines_SpendGross_2+
    ds.prep$UM_AllChan_Brand_Newspapers_SpendGross_2+
    ds.prep$UM_AllChan_Brand_OOH_SpendGross_2+
    ds.prep$UM_AllChan_Brand_Radio_SpendGross_2+
    ds.prep$UM_AllChan_Brand_TradePress_SpendGross_2+
    ds.prep$UM_AllChan_Brand_TV_SpendGross_2
)

ds.prep$UM_AllChan_HSIn_All_SpendGross_2 <- (
#    ds.prep$UM_AllChan_HSIn_Internet_SpendGross_2+
    ds.prep$UM_AllChan_HSIn_Magazines_SpendGross_2+
    ds.prep$UM_AllChan_HSIn_Newspapers_SpendGross_2+
    ds.prep$UM_AllChan_HSIn_OOH_SpendGross_2+
    ds.prep$UM_AllChan_HSIn_Radio_SpendGross_2+
    ds.prep$UM_AllChan_HSIn_TradePress_SpendGross_2+
    ds.prep$UM_AllChan_HSIn_TV_SpendGross_2
)

ds.prep$UM_AllChan_Total_All_SpendGross_2 <- ds.prep$UM_AllChan_HSIn_All_SpendGross_2 + ds.prep$UM_AllChan_Brand_All_SpendGross_2

ds.prep$UM_AllChan_2P_PaidSearch_Impressions_Product_Discount_Weighted <- ds.prep$UM_AllChan_2P_PaidSearch_Impressions_Product * ds.prep$UM_AllChan_2P_EffectivePriceRed_Euros

# lag media spend variables by 2 weeks. i.e value from 2 weeks ago becomes current variable

library(DataCombine)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_Brand_All_SpendGross_2", NewVar = "UM_AllChan_Brand_All_SpendGross_2L2",slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_Brand_All_SpendGross_2", NewVar = "UM_AllChan_Brand_All_SpendGross_2L1",slideBy = -1)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_HSIn_All_SpendGross_2", NewVar = "UM_AllChan_HSIn_All_SpendGross_2L2",slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_Brand_Internet_SpendGross_2", NewVar = "UM_AllChan_Brand_Internet_SpendGross_2L2", slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_Total_All_SpendGross_2", NewVar = "UM_AllChan_Total_All_SpendGross_2L2", slideBy = -2)

# brand media individual
ds.prep <- slide(ds.prep, Var = "UM_AllChan_Brand_Magazines_SpendGross_2", NewVar = "UM_AllChan_Brand_Magazines_SpendGross_2L2",slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_Brand_Newspapers_SpendGross_2", NewVar = "UM_AllChan_Brand_Newspapers_SpendGross_2L2",slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_Brand_OOH_SpendGross_2", NewVar = "UM_AllChan_Brand_OOH_SpendGross_2L2",slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_Brand_Radio_SpendGross_2", NewVar = "UM_AllChan_Brand_Radio_SpendGross_2L2",slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_Brand_TradePress_SpendGross_2", NewVar = "UM_AllChan_Brand_TradePress_SpendGross_2L2",slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_Brand_TV_SpendGross_2", NewVar = "UM_AllChan_Brand_TV_SpendGross_2L2",slideBy = -2)

# product media indidividual
ds.prep <- slide(ds.prep, Var = "UM_AllChan_HSIn_Magazines_SpendGross_2", NewVar = "UM_AllChan_HSIn_Magazines_SpendGross_2L2",slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_HSIn_Newspapers_SpendGross_2", NewVar = "UM_AllChan_HSIn_Newspapers_SpendGross_2L2",slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_HSIn_OOH_SpendGross_2", NewVar = "UM_AllChan_HSIn_OOH_SpendGross_2L2",slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_HSIn_Radio_SpendGross_2", NewVar = "UM_AllChan_HSIn_Radio_SpendGross_2L2",slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_HSIn_TradePress_SpendGross_2", NewVar = "UM_AllChan_HSIn_TradePress_SpendGross_2L2",slideBy = -2)
ds.prep <- slide(ds.prep, Var = "UM_AllChan_HSIn_TV_SpendGross_2", NewVar = "UM_AllChan_HSIn_TV_SpendGross_2L2",slideBy = -2)

# Recode NAs as zeroes
ds.prep$UM_AllChan_Brand_All_SpendGross_2L2[is.na(ds.prep$UM_AllChan_Brand_All_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_Brand_All_SpendGross_2L1[is.na(ds.prep$UM_AllChan_Brand_All_SpendGross_2L1)] <- 0
ds.prep$UM_AllChan_HSIn_All_SpendGross_2L2[is.na(ds.prep$UM_AllChan_HSIn_All_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_Brand_Internet_SpendGross_2L2[is.na(ds.prep$UM_AllChan_Brand_Internet_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_Total_All_SpendGross_2L2[is.na(ds.prep$UM_AllChan_Total_All_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_AllProd_ServiceUpdate_Stepchange_TVrelated[is.na(ds.prep$UM_AllChan_AllProd_ServiceUpdate_Stepchange_TVrelated)] <- 0

ds.prep$UM_AllChan_Brand_Magazines_SpendGross_2L2[is.na(ds.prep$UM_AllChan_Brand_Magazines_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_Brand_Newspapers_SpendGross_2L2[is.na(ds.prep$UM_AllChan_Brand_Newspapers_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_Brand_OOH_SpendGross_2L2[is.na(ds.prep$UM_AllChan_Brand_OOH_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_Brand_Radio_SpendGross_2L2[is.na(ds.prep$UM_AllChan_Brand_Radio_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_Brand_TradePress_SpendGross_2L2[is.na(ds.prep$UM_AllChan_Brand_TradePress_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_Brand_TV_SpendGross_2L2[is.na(ds.prep$UM_AllChan_Brand_TV_SpendGross_2L2)] <- 0

ds.prep$UM_AllChan_HSIn_Magazines_SpendGross_2L2[is.na(ds.prep$UM_AllChan_HSIn_Magazines_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_HSIn_Newspapers_SpendGross_2L2[is.na(ds.prep$UM_AllChan_HSIn_Newspapers_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_HSIn_OOH_SpendGross_2L2[is.na(ds.prep$UM_AllChan_HSIn_OOH_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_HSIn_Radio_SpendGross_2L2[is.na(ds.prep$UM_AllChan_HSIn_Radio_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_HSIn_TradePress_SpendGross_2L2[is.na(ds.prep$UM_AllChan_HSIn_TradePress_SpendGross_2L2)] <- 0
ds.prep$UM_AllChan_HSIn_TV_SpendGross_2L2[is.na(ds.prep$UM_AllChan_HSIn_TV_SpendGross_2L2)] <- 0
