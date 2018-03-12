
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
  
#----------------------------------------------------------------------------

# check
test_var <- "UM_AllChan_HSIn_Radio_GRP"
output_list[[test_var]]$adstock_params
adGRP_adstocked[,test_var] - adstock_it(ds.prep[,test_var], 0.4, 0, 100)
cor(adGRP_adstocked[,test_var], dep_var)
output_list[[test_var]]$results

plot(1:length(var), scale(ds.prep[,test_var]), type = "o", xlab = "time", ylab = "scaled Value")
lines(1:length(var), adGRP_adstocked[,test_var], col = "blue")


