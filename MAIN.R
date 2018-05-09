
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

ds.prep <- read.csv("./Data/Model_Database_20180504_Until KW201752_Renamed_Columns.csv")
datadict <- read.csv("./Data/datadict_v2.csv") 

ds.prep$UM_AllChan_Brand_All_SpendGross_2 <- (
    ds.prep$UM_AllChan_Brand_Internet_SpendGross_2+
    ds.prep$UM_AllChan_Brand_Magazines_SpendGross_2+
    ds.prep$UM_AllChan_Brand_Newspapers_SpendGross_2+
    ds.prep$UM_AllChan_Brand_OOH_SpendGross_2+
    ds.prep$UM_AllChan_Brand_Radio_SpendGross_2+
    ds.prep$UM_AllChan_Brand_TradePress_SpendGross_2+
    ds.prep$UM_AllChan_Brand_TV_SpendGross_2
)

ds.prep$UM_AllChan_HSIn_All_SpendGross_2 <- (
    ds.prep$UM_AllChan_HSIn_Internet_SpendGross_2+
    ds.prep$UM_AllChan_HSIn_Magazines_SpendGross_2+
    ds.prep$UM_AllChan_HSIn_Newspapers_SpendGross_2+
    ds.prep$UM_AllChan_HSIn_OOH_SpendGross_2+
    ds.prep$UM_AllChan_HSIn_Radio_SpendGross_2+
    ds.prep$UM_AllChan_HSIn_TradePress_SpendGross_2+
    ds.prep$UM_AllChan_HSIn_TV_SpendGross_2
)
