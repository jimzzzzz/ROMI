# This runs a random forest on all variables in the selection set to give an indication of variable importance
# We sort the output based on meanImportance and add variables one by one into the model
# and keep them if the betas have the right sign and the pvalue is good

set.seed(123)
boruta.train <- Boruta(target~., data=ds.prep.model, doTrace = 2)
final.boruta <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(final.boruta)
class(boruta.df)
View(boruta.df)
