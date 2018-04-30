 
# Define target and independents ------------------------------------------
# start with seasonals - pricing - competitive marketing - operational/structural events

# Data for refresh
ds.prep <- read.csv("./Data/Model_Database_2018-01-22_v1.csv", 
                              sep = ",", 
                              dec = ".", 
                              stringsAsFactors=FALSE)

# Linear Regression
ds.prep$UM_AllChan_Internet_Radio_SpendGross_AD7          <- adstock_it(ds.prep$UM_AllChan_Internet_Radio_SpendGross      , 0.1, 0.35, 500)
ds.prep$UM_AllChan_Internet_TV_SpendGross_AD7             <- adstock_it(ds.prep$UM_AllChan_Internet_TV_SpendGross         , 0.1, 0.35, 500)
ds.prep$UM_AllChan_Internet_TradePress_SpendGross_AD7     <- adstock_it(ds.prep$UM_AllChan_Internet_TradePress_SpendGross , 0.1, 0.35, 100)
ds.prep$UM_AllChan_Internet_Magazine_SpendGross_AD7       <- adstock_it(ds.prep$UM_AllChan_Internet_Magazine_SpendGross   , 0.1, 0.35, 500)
ds.prep$UM_AllChan_Internet_OOH_SpendGross_AD7            <- adstock_it(ds.prep$UM_AllChan_Internet_OOH_SpendGross        , 0.1, 0.35, 500)
ds.prep$UM_AllChan_Internet_NewsPaper_SpendGross_AD7      <- adstock_it(ds.prep$UM_AllChan_Internet_NewsPaper_SpendGross  , 0.1, 0.35, 500)
ds.prep$UM_AllChan_Internet_Cinema_SpendGross_AD7         <- adstock_it(ds.prep$UM_AllChan_Internet_Cinema_SpendGross     , 0.1, 0.35, 500)

# ds.prep$UM_AllChan_Internet_Radio_SpendGross          <- adstock_it(ds.prep$UM_AllChan_Internet_Radio_SpendGross      , 0.1, 0.35, 500)
# ds.prep$UM_AllChan_Internet_TV_SpendGross             <- adstock_it(ds.prep$UM_AllChan_Internet_TV_SpendGross         , 0.1, 0.35, 500)
# ds.prep$UM_AllChan_Internet_TradePress_SpendGross     <- adstock_it(ds.prep$UM_AllChan_Internet_TradePress_SpendGross , 0.1, 0.35, 100)
# ds.prep$UM_AllChan_Internet_Magazine_SpendGross       <- adstock_it(ds.prep$UM_AllChan_Internet_Magazine_SpendGross   , 0.1, 0.35, 500)
# ds.prep$UM_AllChan_Internet_OOH_SpendGross            <- adstock_it(ds.prep$UM_AllChan_Internet_OOH_SpendGross        , 0.1, 0.35, 500)
# ds.prep$UM_AllChan_Internet_NewsPaper_SpendGross      <- adstock_it(ds.prep$UM_AllChan_Internet_NewsPaper_SpendGross  , 0.1, 0.35, 500)
# ds.prep$UM_AllChan_Internet_Cinema_SpendGross         <- adstock_it(ds.prep$UM_AllChan_Internet_Cinema_SpendGross     , 0.1, 0.35, 500)


n <- c(  # Zielvariable

  "UM_Digitaldir_2P_Sales_Units_OE"

  ,"UM_AllChan_2P_ListPrice_Dummy"
  ,"UM_AllChan_MSI_Discount15End_Dummy"
  ,"UM_AllChan_MSI_PostDiscount15_Dummy"

  # ,"UM_AllChan_Internet_Radio_SpendGross"
  #,"UM_AllChan_Internet_TV_SpendGross"
  #,"UM_AllChan_Internet_TradePress_SpendGross"
  #,"UM_AllChan_Internet_Magazine_SpendGross"
  #,"UM_AllChan_Internet_OOH_SpendGross"
  #,"UM_AllChan_Internet_NewsPaper_SpendGross"
  #,"UM_AllChan_Internet_Cinema_SpendGross"

  ,"UM_HSIn_AllProd_TraditionalMedia_SpendGross_AD7"

  ,"UM_AllChan_Non_B2B_sum_Impressions_AD7"
  ,"UM_AllChan_AllProd_NonProgDisplay_Impressions_T_AD7"
  ,"UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_Discount_Weighted_AD7"
  ,"UM_AllChan_2P_SocialFBInsta_Impressions_T_AD7"
  ,"Comp_AllChan_Media_Spend_AD7"
  ,"Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6"
  ,"UM_AllChan_2P_ScarcityIndicator_StepChange"
)

# n <- c("UM_Digitaldir_2P_Sales_Units_OE",
# 
#        "UM_AllChan_2P_ListPrice_Dummy",
#        "UM_AllChan_MSI_Discount15End_Dummy",
#        "UM_AllChan_MSI_PostDiscount15_Dummy",
#        "UM_AllChan_Internet_Radio_SpendGross",
#        "UM_AllChan_Internet_TV_SpendGross",
#        "UM_AllChan_Internet_TradePress_SpendGross",
#        "UM_AllChan_Internet_Magazine_SpendGross",
#        "UM_AllChan_Internet_OOH_SpendGross",
#        "UM_AllChan_Internet_NewsPaper_SpendGross",
#        "UM_AllChan_Internet_Cinema_SpendGross",
#        "UM_AllChan_Non_B2B_sum_Impressions",
#        "UM_AllChan_AllProd_NonProgDisplay_Impressions_T",
#        "UM_AllChan_2P_PaidSearch_Impressions_Non-Brand_Product_Discount_Weighted",
#        "UM_AllChan_2P_SocialFB&Insta_Impressions_T",
#        "Comp_AllChan_Media_Spend",
#        "Comp_AllChan_2P_Effectively_Mtl_Price_Avg_Ranking_V6",
#        "UM_AllChan_2P_ScarcityIndicator_StepChange"
# )
# 
# n <- c("UM_HSIn_AllProd_TraditionalMedia_SpendGross",
#        "UM_AllChan_Non_B2B_sum_Impressions",
#        "UM_AllChan_AllProd_NonProgDisplay_Impressions_T",
#        "UM_AllChan_2P_PaidSearch_Impressions_NonBrand_Product_Discount_Weighted",
#        "UM_AllChan_2P_SocialFBInsta_Impressions_T",
#        "Comp_AllChan_Media_Spend"
# )

n <- n[(n %in% colnames(ds.prep))] # takes only variables which exist in our data

ds.prep.model <- ds.prep[,names(ds.prep) %in% n]



