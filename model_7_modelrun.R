
a <- DLMModel(ds.prep.model, 100, vLevel = -1)

debug(DLMModel)

a$model_base
a$model_beta
a$pVal
contributions <- a$contributions
INPUT <- a$INPUT

lm1 <- lm(UM_Digitaldir_2P_Sales_Units_OE ~ ., data = ds.prep.model)
predict(lm1)
summary(lm1)

ind.Var <- ds.prep.model[,2:ncol(ds.prep.model)]
model_beta <- a$model_beta

a <- sweep(ind.Var, 2, model_beta, "*") 
