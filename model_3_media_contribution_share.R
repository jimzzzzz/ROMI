# how to distribute contribution of 

contrib <- contributions$UM_AllChan_HSIn_All_SpendGross_2L2

# need to adstock components with same scalar. Let's just use same adstock as used with aggregate
# this gives the parameters (0.1, 0.1, 100)
output_list$UM_AllChan_HSIn_All_SpendGross_2L2$adstock_params

var_adstocked$UM_AllChan_HSIn_Radio_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_HSIn_Radio_SpendGross_2L2, 0.1, 0.1, 100)
var_adstocked$UM_AllChan_HSIn_TV_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_HSIn_TV_SpendGross_2L2, 0.1, 0.1, 100)
var_adstocked$UM_AllChan_HSIn_TradePress_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_HSIn_TradePress_SpendGross_2L2, 0.1, 0.1, 100)
var_adstocked$UM_AllChan_HSIn_Magazines_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_HSIn_Magazines_SpendGross_2L2, 0.1, 0.1, 100)
var_adstocked$UM_AllChan_HSIn_OOH_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_HSIn_OOH_SpendGross_2L2, 0.1, 0.1, 100)
var_adstocked$UM_AllChan_HSIn_Newspapers_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_HSIn_Newspapers_SpendGross_2L2, 0.1, 0.1, 100)

cs<-c("UM_AllChan_HSIn_Radio_SpendGross_2L2", 
      "UM_AllChan_HSIn_TV_SpendGross_2L2",
      "UM_AllChan_HSIn_TradePress_SpendGross_2L2", 
      "UM_AllChan_HSIn_Magazines_SpendGross_2L2",
      "UM_AllChan_HSIn_OOH_SpendGross_2L2", 
      "UM_AllChan_HSIn_Newspapers_SpendGross_2L2"
)

# make new dataframe

media_contrib <- cbind(contrib, var_adstocked[,names(var_adstocked) %in% cs])

media_contrib$Total <- var_adstocked$UM_AllChan_HSIn_Radio_SpendGross_2L2+
  var_adstocked$UM_AllChan_HSIn_TV_SpendGross_2L2+
  var_adstocked$UM_AllChan_HSIn_TradePress_SpendGross_2L2+
  var_adstocked$UM_AllChan_HSIn_Magazines_SpendGross_2L2+
  var_adstocked$UM_AllChan_HSIn_OOH_SpendGross_2L2+
  var_adstocked$UM_AllChan_HSIn_Newspapers_SpendGross_2L2

media_contrib$share_Radio <- media_contrib$contrib*media_contrib$UM_AllChan_HSIn_Radio_SpendGross_2L2/media_contrib$Total
media_contrib$share_TV <- media_contrib$contrib*media_contrib$UM_AllChan_HSIn_TV_SpendGross_2L2/media_contrib$Total
media_contrib$share_TradePress <- media_contrib$contrib*media_contrib$UM_AllChan_HSIn_TradePress_SpendGross_2L2/media_contrib$Total
media_contrib$share_Magazines <- media_contrib$contrib*media_contrib$UM_AllChan_HSIn_Magazines_SpendGross_2L2/media_contrib$Total
media_contrib$share_OOH <- media_contrib$contrib*media_contrib$UM_AllChan_HSIn_OOH_SpendGross_2L2/media_contrib$Total
media_contrib$share_Newspapers <- media_contrib$contrib*media_contrib$UM_AllChan_HSIn_Newspapers_SpendGross_2L2/media_contrib$Total

co <-c("contrib", "share_Radio", "share_TV", "share_TradePress", "share_Magazines", "share_OOH", "share_Newspapers")

OUTPUT <- t(media_contrib[,names(media_contrib) %in% co])
