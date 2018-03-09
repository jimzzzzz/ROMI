
# Before starting to run the code a project from R studio should be created
# in the folder where the R files are. When the project is opened the working
# directory should be the location of the files and there should not be any 
# issues with the file paths
#
# This file should be run first. It is loading the required libraries and helper functions
# After runing it the programer should proceed with the files that he would work on

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

#-----------------------------------------------------------------------------------------------

# functions that would be used throughout the whole project are sourced here
source("helper_functions.R")

#-----------------------------------------------------------------------------------------------

# Data import
ds.prep <- read.csv("./Data/Model_Database_2018-01-22_v1.csv")



