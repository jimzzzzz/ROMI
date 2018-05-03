# Model 7 - Digital indirect 2P

# Read in data ------------------------------------------------------------

ds.prep <- read.csv("./Data/Model_Database_20180419_Until KW201752_SEM_Trad_Reworked.csv")
datadict <- read.csv("./Data/datadict.csv") 

# Define target ------------------------------------------

target <- ds.prep$UM_Digitaldir_2P_Sales_Units_OE

# Run adstocks ------------------------------------------------------------

source("adstock all.R")

# Go to model_7_variable_selection ----------------------------------------
