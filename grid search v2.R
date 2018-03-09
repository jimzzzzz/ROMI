
#-------------------------------------------------------------------------------

B <- expand.grid(learn = seq(0, 1, 0.2), decay = seq(0,1, 0.2), scalar = c(100,500,1000))

# Select only GRP variables
datadict <- read.csv("./Data/datadict.csv")[, c('Variable_Name', 'Category')]

GRP <- datadict[which(datadict$Category=='GRP'),]
# |datadict$Category=='Spend'|datadict$Category=='Digital'|datadict$Category=='DM'|datadict$Category=='Comp'),]

# create a vector of GRP variables
vGRP <- as.character(GRP[, "Variable_Name"])

# select GRP variables from main data to be adstocked
adGRP <- ds.prep[,names(ds.prep) %in% vGRP]


# run all these variables through adtsock grid search above and create table of the best adstocks
# then create these adstocks for each variable

#-------------------------------------------------------------------------------

# varnames <- c("UM_AllChan_3p_Outbound_ReachedContacts") # define variables to be adstocked in this vector

varnames <- vGRP

dep_var <- ds.prep[,"UM_TeleinSiS_3P_Sales_Units_OE"] # the variable on which the correlation with the adstocked variables is measured

var <- ds.prep[,varnames[1]] # a variable that could potentially be used for looping if other variables are added in varnames

# some idea for the looping code

adGRP_adstocked <- adGRP
output_list <- list()
for(i in varnames){
  print(i)
  output_list[[i]] <- best_adstock(ds.prep[, i], dep_var, B)

  # or if you want to change it in the data
  adGRP_adstocked[, i] <- output_list[[i]]$adstocked_vector_max
}
  
i <- "UM_AllChan_Mob_Radio_GRP"  

debug(best_adstock)

# Testing
#------------------------------------------------------------------

a <- best_adstock(var, dep_var)
a$adstocked_vector_max # the transformed vector with the best adstock parameters
# View(a$results)
a$adstock_params # the best adstock parameters

# check
a$adstocked_vector_max - adstock_it(var, 0.2, 0, 100)

plot(1:length(var), scale(var), type = "o", xlab = "time", ylab = "scaled Value")
lines(1:length(var), a$adstocked_vector_max, col = "blue")

cor(dep_var, a$adstocked_vector_max)
