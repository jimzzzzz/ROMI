# D2D - can the models be improved by adding a week variable
# First plot the data

peakweek <- read.csv("./Data/peakweek.csv") 
np<-c("KW_Year.Wk", 
     "UM_D2DTeleout_3P_Sales_Units_OE", 
     "UM_D2DTeleout_2P_Sales_Units_OE")

plot.db <- cbind(ds.prep[,names(ds.prep) %in% np], peakweek)

# Plot
library(ggplot2)
# Basic line plot with points
ggplot(plot.db, aes(KW_Year.Wk, UM_D2DTeleout_3P_Sales_Units_OE, group=1)) +
  geom_line()+
  geom_point(aes(color=UM_PeakWeek_Per_Month))

qplot(x=KW_Year.Wk, y=UM_D2DTeleout_2P_Sales_Units_OE, color=UM_PeakWeek_Per_Month, data = plot.db, geom = "point")


ggplot(data=plot.db, aes(x=KW_Year.Wk, y=UM_D2DTeleout_2P_Sales_Units_OE, group=1)) +
  geom_line(color="red")+
  geom_point()

plot.db$UM_D2D_2P_Smooth <- loess(ds.prep$UM_D2DTeleout_2P_Sales_Units_OE~ds.prep$KW_Year.Wk)