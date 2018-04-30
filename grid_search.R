library(data.table)

# create all adstock combinations of single variable
# define combinations - B

B <- expand.grid(learn = seq(0,1,0.2), decay = seq(0,1, 0.2), scalar = c(100,500,1000))

# define variable to be adstocked

var <- ds.prep$UM_AllChan_3p_Outbound_ReachedContacts

UM_TeleinSiS_3P_Sales_Units_OE

# define adstock function

adstock_it <- function(vector, learn, decay, scalar){
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

output <- list() # defines output as a list

# loop to create all adstock combinations

# results <- data.frame()
# colnames(results) <- c("i", "Adstock", "Score")
# colMax <- function(data) sapply(data, max, na.rm = TRUE)

for(i in 1 :nrow(B)){
  print(i)
  vector <- var
  learn  <- B$learn[i]
  decay  <- B$decay[i]
  scalar <- B$scalar[i]
  adstock_name <- paste(learn, decay, scalar, sep="_")
  adstocked_vector <- adstock_it(vector, learn, decay, scalar)
  res <- cor.test(ds.prep$UM_TeleinSiS_3P_Sales_Units_OE, adstocked_vector, method = "pearson") 
  output <- data.frame(i, adstock_name, res$statistic)
  colnames(output) <- c("i", "Adstock", "Score")
  results <- rbind(results, output)
# so choose the adstock with the best score: where max(abs(results$Score), na.rm = TRUE)
# want output to be variable name, adstock parameter  
}

## Apply this procedure to group of variables

# Select only GRP variables
datadict <- read.csv("datadict.csv")[,c('Variable_Name', 'Category')]

GRP <- datadict[which(datadict$Category=='GRP'),]
# |datadict$Category=='Spend'|datadict$Category=='Digital'|datadict$Category=='DM'|datadict$Category=='Comp'),]

# create a vector of GRP variables
vGRP <- GRP[, "Variable_Name"]

# select GRP variables from main data to be adstocked
adGRP <- ds.prep[,names(ds.prep) %in% vGRP]

# run all these variables through adtsock grid search above and create table of the best adstocks
# then create these adstocks for each variable