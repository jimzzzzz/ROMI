
#--------------------------

#--------------------------

options(scipen= 999, digits=8)

library(dlm)
library(car)
library(data.table)
library(ggplot2)
library(lattice)
library(knitr)
library(stringr)
library(reshape2)
library(tidyr)
library(dplyr)
# library(gdata)
library(openxlsx)

#-----------------------------------------------------------------------------------------------

# functions that would be used throughout the whole project are sourced here
source("helper_functions.R")

#-----------------------------------------------------------------------------------------------

# Data import
ds.prep <- read.csv("./Data/Model_Database_2018-01-22_v1.csv")

init_data <- read.xlsx("./Data/Model_Database_20180316_Until KW201743.xlsx",
                       sheet = "DataImport")


