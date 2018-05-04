# Combining media brand and product does not help

ds.prep.media$UM_AllChan_Brand_Internet_SpendGross <- ds.prep$UM_AllChan_AllProd_Internet_SpendGross + ds.prep$UM_AllChan_2P_Internet_SpendGross + ds.prep$UM_AllChan_3P_Internet_SpendGross
ds.prep.media$UM_AllChan_Brand_InternetSocialIO_SpendGross <- ds.prep$UM_AllChan_AllProd_InternetSocialIO_SpendGross + ds.prep$UM_AllChan_2P_InternetSocialIO_SpendGross + ds.prep$UM_AllChan_3P_InternetSocialIO_SpendGross
ds.prep.media$UM_AllChan_Brand_Magazines_SpendGross <- ds.prep$UM_AllChan_AllProd_Magazines_SpendGross + ds.prep$UM_AllChan_2P_Magazines_SpendGross + ds.prep$UM_AllChan_3P_Magazines_SpendGross
ds.prep.media$UM_AllChan_Brand_Newspapers_SpendGross <- ds.prep$UM_AllChan_AllProd_Newspapers_SpendGross + ds.prep$UM_AllChan_2P_Newspapers_SpendGross + ds.prep$UM_AllChan_3P_Newspapers_SpendGross
ds.prep.media$UM_AllChan_Brand_OOH_SpendGross <- ds.prep$UM_AllChan_AllProd_OOH_SpendGross + ds.prep$UM_AllChan_2P_OOH_SpendGross + ds.prep$UM_AllChan_3P_OOH_SpendGross
ds.prep.media$UM_AllChan_Brand_Radio_SpendGross <- ds.prep$UM_AllChan_AllProd_Radio_SpendGross + ds.prep$UM_AllChan_2P_Radio_SpendGross + ds.prep$UM_AllChan_3P_Radio_SpendGross
ds.prep.media$UM_AllChan_Brand_TradePress_SpendGross <- ds.prep$UM_AllChan_AllProd_TradePress_SpendGross + ds.prep$UM_AllChan_2P_TradePress_SpendGross + ds.prep$UM_AllChan_3P_TradePress_SpendGross
ds.prep.media$UM_AllChan_Brand_TV_SpendGross <- ds.prep$UM_AllChan_AllProd_TV_SpendGross + ds.prep$UM_AllChan_2P_TV_SpendGross + ds.prep$UM_AllChan_3P_TV_SpendGross
ds.prep.media$UM_AllChan_Brand_All_SpendGross <- (ds.prep.media$UM_AllChan_Brand_Internet_SpendGross
                                                + ds.prep.media$UM_AllChan_Brand_InternetSocialIO_SpendGross
                                                + ds.prep.media$UM_AllChan_Brand_Magazines_SpendGross
                                                + ds.prep.media$UM_AllChan_Brand_Newspapers_SpendGross
                                                + ds.prep.media$UM_AllChan_Brand_OOH_SpendGross
                                                + ds.prep.media$UM_AllChan_Brand_Radio_SpendGross
                                                + ds.prep.media$UM_AllChan_Brand_TradePress_SpendGross
                                                + ds.prep.media$UM_AllChan_Brand_TV_SpendGross)


new_media <- c("UM_AllChan_Brand_Internet_SpendGross",
               "UM_AllChan_Brand_InternetSocialIO_SpendGross",
               "UM_AllChan_Brand_Magazines_SpendGross",
               "UM_AllChan_Brand_Newspapers_SpendGross",
               "UM_AllChan_Brand_OOH_SpendGross",
               "UM_AllChan_Brand_Radio_SpendGross",
               "UM_AllChan_Brand_TradePress_SpendGross",
               "UM_AllChan_Brand_TV_SpendGross",
               "UM_AllChan_Brand_All_SpendGross")

ad_vars1 <- ds.prep.media[,names(ds.prep) %in% new_media]

var_adstocked1 <- ad_vars1

output_list <- list()
for(i in new_media){
  print(i)
  output_list[[i]] <- best_adstock(ds.prep.media[, i], target, B)
  
  # or if you want to change it in the data
  var_adstocked1[, i] <- output_list[[i]]$adstocked_vector_max
}

ds.prep.model <- cbind(target, var_adstocked1[,names(var_adstocked1) %in% new_media])


set.seed(123)
boruta.train <- Boruta(target~., data=ds.prep.model, doTrace = 2)
final.boruta <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(final.boruta)
class(boruta.df)
View(boruta.df)

ns<-c("UM_AllChan_Brand_TradePress_SpendGross", "UM_AllChan_Brand_Magazines_SpendGross"
)
ds.prep.model<-cbind(target, ds.prep[,names(ds.prep) %in% nm], var_adstocked1[,names(var_adstocked1) %in% new_media])

scaling_factor <- 100
vLevel <- NULL
source("model code.R")
View(model_beta)
View(pVal)


