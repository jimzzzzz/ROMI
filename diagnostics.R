#
# Progress the Data
#
coeff     <- kFilterSmooth$s[2 : 2, 1 : (ncol(kFilterSmooth$s) - 1)]
intercept <- kFilterSmooth$s[2 : nrow(kFilterSmooth$s), (ncol(kFilterSmooth$s)) ] 

dim(coeff)
length(intercept)

#A <- coeff * ind.Var   * scaling_factor
A <- sweep(ind.Var, MARGIN = 2, coeff, "*") * scaling_factor
intercept <- intercept * scaling_factor

y     <- dep.Var * scaling_factor
y_hat <- rowSums(A) + intercept

Overview <- as.data.table(cbind(coeff * scaling_factor, base = intercept, y_hat, y, resi = (y - y_hat)))

Overview$resi2 <- 0

for(i in 2 : (nrow(Overview) - 1) ){
  
  Overview$resi2[i] <- min( abs(Overview$y[i] - Overview$y_hat[i])
                            , abs(Overview$y[i] - Overview$y_hat[i-1])
                            , abs(Overview$y[i] - Overview$y_hat[i+1]))
  
}

#View(Overview)


#
# Residuals
#

qqnorm(Overview$resi, ylim = c(-400, 800))

summary(Overview$resi)

plot(Overview$resi, ylim = c(-400, 800))

plot(Overview$y, Overview$resi, xlab = "Dependent Variable", ylab = "residual", main = "Residual vs Real", ylim = c(-400, 800))

plot(Overview$y_hat, Overview$resi, xlab = "Estimate", ylab = "residual", main = "Residual vs Estimate", ylim = c(-400, 800), xlim = c(0, 5000))

boxplot(Overview$resi)

#
# R2
#


R2 <- 1 - (sum((Overview$y-Overview$y_hat )^2)/sum((Overview$y-mean(Overview$y))^2))
R2

distance1 <- sum(abs(Overview$resi))
distance1

distance2 <- sum(Overview$resi2)
distance2

#
# Plot
#
date <-ds.prep$KW_Year.Wk
plot.db <- data.frame(cbind(Overview, date =  seq(1,156,1) ))

(ggplot(plot.db, aes(x=date, y=resi)) 
  + geom_line(aes(x=date, y=y_hat, color = "predicted", group = 1), colour="red") + geom_point(aes(x=date, y=y_hat),shape = 1,size = 2)
  + geom_line(aes(x=date, y=y    , color = "real value", group = 1), colour="blue" ) + geom_point(aes(x=date, y=y),shape = 0,size = 2)
  + geom_line(aes(x=date, y=(base) , color = "Base", group = 1), colour="black" )
  + geom_bar(stat="identity")
)

rownames(A) <- NULL

colnames(A) <- colnames(ind.Var)

A <- cbind(A, base = Overview$base, date = plot.db$date)

A_hat <- melt(A, id = c("date"))

A_hat_minus <- A_hat[A_hat$value >= 0,]

A_hat_plus  <- A_hat[A_hat$value <  0,]

View(cbind(A, base = Overview$base, pred = Overview$y_hat, real = Overview$y))

A <- cbind(A, base = Overview$base, pred = Overview$y_hat, real = Overview$y)


(ggplot(A_hat, aes(x = date, y = value, fill = variable)) + geom_area(colour = "black", size = .2, alpha = 0.4) ) 


(ggplot() 
  + geom_bar(data = A_hat_minus, aes(x = date, y = value, fill = variable), stat = "identity")
  + geom_bar(data = A_hat_plus, aes(x = date, y = value, fill = variable), stat = "identity")
  + geom_area(colour = "black", size = .2, alpha = 0.4) ) 




#
# Save the Analytic
#

ordner    = "X:/07_Projects/36_ROMI/Code_Simon/"
modelname = "Model3/"
prefix    = "model3_"

#
# Save the Resi Plot
#

pdf(paste(ordner,modelname,prefix,"resi.pdf" ,sep = ""))

plot(Overview$resi, ylim = c(-400, 800))

dev.off()



#
# Save the Resi vs Real Plot
#

pdf(paste(ordner,modelname,prefix,"resi_vs_real.pdf" ,sep = ""))

plot(Overview$y, Overview$resi, xlab = "Dependent Variable", ylab = "residual", main = "Residual vs Real", ylim = c(-400, 800))

dev.off()


#
# Save the Resi vs Estimate Plot
#

pdf( paste(ordner,modelname,prefix,"resi_vs_estimate.pdf" ,sep = ""))

plot(Overview$y_hat, Overview$resi, xlab = "Estimate", ylab = "residual", main = "Residual vs Estimate", ylim = c(-400, 800))

dev.off()


#
# Save the Model Plot
#

pdf( paste(ordner,modelname,prefix,"model.pdf" ,sep = ""))

(ggplot(plot.db, aes(x=date, y=resi)) 
  + geom_line(aes(x=date, y=y_hat, color = "predicted", group = 1), colour="red") + geom_point(aes(x=date, y=y_hat),shape = 1,size = 2)
  + geom_line(aes(x=date, y=y    , color = "real value", group = 1), colour="blue" ) + geom_point(aes(x=date, y=y),shape = 0,size = 2)
  + geom_line(aes(x=date, y=(base) , color = "Base", group = 1), colour="black" )
  + geom_bar(stat="identity")
)

dev.off()


#
# Save the betas
#

# Save the betas
write.csv2(model_beta, file = paste(ordner,modelname,prefix,"beta.csv",sep = "") )

# Save the matrix A
write.csv2(A, file = paste(ordner,modelname,prefix,"A.csv",sep = "") )

# Save the matrix Overview
write.csv2(Overview, file = paste(ordner,modelname,prefix,"Overview.csv",sep = ""))


pdf( paste(ordner,modelname,prefix,"stacked_lines.pdf" ,sep = ""))

(ggplot(A_hat, aes(x = date, y = value, fill = variable)) + geom_area(colour = "black", size = .2, alpha = 0.4) ) 

dev.off()


pdf( paste(ordner,modelname,prefix,"stacked_lines_nolegend.pdf" ,sep = ""))

(ggplot(A_hat, aes(x = date, y = value, fill = variable)) + geom_area(colour = "black", size = .2, alpha = 0.4) ) + guides(fill = F)

dev.off()



pdf( paste(ordner,modelname,prefix,"stacked_bar.pdf" ,sep = ""))

(ggplot() 
  + geom_bar(data = A_hat_minus, aes(x = date, y = value, fill = variable), stat = "identity")
  + geom_bar(data = A_hat_plus, aes(x = date, y = value, fill = variable), stat = "identity")
  + geom_area(colour = "black", size = .2, alpha = 0.4) ) 

dev.off()
