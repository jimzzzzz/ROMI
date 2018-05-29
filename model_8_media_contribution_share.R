# how to distribute contribution of 

contrib <- contributions$UM_AllChan_Total_All_SpendGross_2L2

# need to adstock components with same scalar. Let's just use same adstock as used with aggregate
# this gives the parameters (0.3, 0.9, 1000)
output_list$UM_AllChan_Total_All_SpendGross_2L2$adstock_params

var_adstocked$UM_AllChan_HSIn_Radio_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_HSIn_Radio_SpendGross_2L2, 0.3, 0.9, 1000)
var_adstocked$UM_AllChan_HSIn_TV_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_HSIn_TV_SpendGross_2L2, 0.3, 0.9, 1000)
var_adstocked$UM_AllChan_HSIn_TradePress_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_HSIn_TradePress_SpendGross_2L2, 0.3, 0.9, 1000)
var_adstocked$UM_AllChan_HSIn_Magazines_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_HSIn_Magazines_SpendGross_2L2, 0.3, 0.9, 1000)
var_adstocked$UM_AllChan_HSIn_OOH_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_HSIn_OOH_SpendGross_2L2, 0.3, 0.9, 1000)
var_adstocked$UM_AllChan_HSIn_Newspapers_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_HSIn_Newspapers_SpendGross_2L2, 0.3, 0.9, 1000)
var_adstocked$UM_AllChan_Brand_Radio_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_Brand_Radio_SpendGross_2L2, 0.3, 0.9, 1000)
var_adstocked$UM_AllChan_Brand_TV_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_Brand_TV_SpendGross_2L2, 0.3, 0.9, 1000)
var_adstocked$UM_AllChan_Brand_TradePress_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_Brand_TradePress_SpendGross_2L2, 0.3, 0.9, 1000)
var_adstocked$UM_AllChan_Brand_Magazines_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_Brand_Magazines_SpendGross_2L2, 0.3, 0.9, 1000)
var_adstocked$UM_AllChan_Brand_OOH_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_Brand_OOH_SpendGross_2L2, 0.3, 0.9, 1000)
var_adstocked$UM_AllChan_Brand_Newspapers_SpendGross_2L2 <- adstock_it(ds.prep$UM_AllChan_Brand_Newspapers_SpendGross_2L2, 0.3, 0.9, 1000)

cs<-c("UM_AllChan_HSIn_Radio_SpendGross_2L2", 
      "UM_AllChan_HSIn_TV_SpendGross_2L2",
      "UM_AllChan_HSIn_TradePress_SpendGross_2L2", 
      "UM_AllChan_HSIn_Magazines_SpendGross_2L2",
      "UM_AllChan_HSIn_OOH_SpendGross_2L2", 
      "UM_AllChan_HSIn_Newspapers_SpendGross_2L2"
      , "UM_AllChan_Brand_Magazines_SpendGross_2L2"
      , "UM_AllChan_Brand_Newspapers_SpendGross_2L2"
      , "UM_AllChan_Brand_OOH_SpendGross_2L2"
      , "UM_AllChan_Brand_Radio_SpendGross_2L2"
      , "UM_AllChan_Brand_TradePress_SpendGross_2L2"
      , "UM_AllChan_Brand_TV_SpendGross_2L2"
)

# make new dataframe

media_contrib <- cbind(contrib, var_adstocked[,names(var_adstocked) %in% cs])

media_contrib$Total <- var_adstocked$UM_AllChan_HSIn_Radio_SpendGross_2L2+
  var_adstocked$UM_AllChan_HSIn_TV_SpendGross_2L2+
  var_adstocked$UM_AllChan_HSIn_TradePress_SpendGross_2L2+
  var_adstocked$UM_AllChan_HSIn_Magazines_SpendGross_2L2+
  var_adstocked$UM_AllChan_HSIn_OOH_SpendGross_2L2+
  var_adstocked$UM_AllChan_HSIn_Newspapers_SpendGross_2L2+
  var_adstocked$UM_AllChan_Brand_Radio_SpendGross_2L2+
  var_adstocked$UM_AllChan_Brand_TV_SpendGross_2L2+
  var_adstocked$UM_AllChan_Brand_TradePress_SpendGross_2L2+
  var_adstocked$UM_AllChan_Brand_Magazines_SpendGross_2L2+
  var_adstocked$UM_AllChan_Brand_OOH_SpendGross_2L2+
  var_adstocked$UM_AllChan_Brand_Newspapers_SpendGross_2L2

media_contrib$share_Radio_HSIn <- media_contrib$contrib*media_contrib$UM_AllChan_HSIn_Radio_SpendGross_2L2/media_contrib$Total
media_contrib$share_TV_HSIn <- media_contrib$contrib*media_contrib$UM_AllChan_HSIn_TV_SpendGross_2L2/media_contrib$Total
media_contrib$share_TradePress_HSIn <- media_contrib$contrib*media_contrib$UM_AllChan_HSIn_TradePress_SpendGross_2L2/media_contrib$Total
media_contrib$share_Magazines_HSIn <- media_contrib$contrib*media_contrib$UM_AllChan_HSIn_Magazines_SpendGross_2L2/media_contrib$Total
media_contrib$share_OOH_HSIn <- media_contrib$contrib*media_contrib$UM_AllChan_HSIn_OOH_SpendGross_2L2/media_contrib$Total
media_contrib$share_Newspapers_HSIn <- media_contrib$contrib*media_contrib$UM_AllChan_HSIn_Newspapers_SpendGross_2L2/media_contrib$Total
media_contrib$share_Radio_Brand <- media_contrib$contrib*media_contrib$UM_AllChan_Brand_Radio_SpendGross_2L2/media_contrib$Total
media_contrib$share_TV_Brand <- media_contrib$contrib*media_contrib$UM_AllChan_Brand_TV_SpendGross_2L2/media_contrib$Total
media_contrib$share_TradePress_Brand <- media_contrib$contrib*media_contrib$UM_AllChan_Brand_TradePress_SpendGross_2L2/media_contrib$Total
media_contrib$share_Magazines_Brand <- media_contrib$contrib*media_contrib$UM_AllChan_Brand_Magazines_SpendGross_2L2/media_contrib$Total
media_contrib$share_OOH_Brand <- media_contrib$contrib*media_contrib$UM_AllChan_Brand_OOH_SpendGross_2L2/media_contrib$Total
media_contrib$share_Newspapers_Brand <- media_contrib$contrib*media_contrib$UM_AllChan_Brand_Newspapers_SpendGross_2L2/media_contrib$Total

co <-c("contrib", "share_Radio_HSIn", "share_TV_HSIn", "share_TradePress_HSIn", 
       "share_Magazines_HSIn", "share_OOH_HSIn", "share_Newspapers_HSIn", 
       "share_Radio_Brand", "share_TV_Brand", "share_TradePress_Brand", 
       "share_Magazines_Brand", "share_OOH_Brand", "share_Newspapers_Brand")

OUTPUT <- t(media_contrib[,names(media_contrib) %in% co])

