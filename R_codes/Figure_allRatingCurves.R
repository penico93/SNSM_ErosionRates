
# Select directory where data files are stored
directory = "/Users/penico93/Dropbox/Caudales_SNSM/Data"

setwd(directory)

#Figure rating curves and erosion rates

#Script for plotting figure with rating curves used and the times series of sediment yield and waterdischarge used

rating.cantaclaro <- read.csv('Cantaclaro_Data_Ratig_Curve.csv',header = TRUE)

rating.cantaclaro$X <- log(rating.cantaclaro$CAUDAL_LIQUIDO.m3.s.)
rating.cantaclaro$Y <- log(rating.cantaclaro$GASTO_SOLIDO.Kg.s.)

model1 = lm(Y~X, data = rating.cantaclaro)

summary(model1)

#coeficiente de determinacion R cuadrado
summary(model1)$r.squared
                              
min(log(rating.cantaclaro$CAUDAL_LIQUIDO.m3.s.))

max(log(rating.cantaclaro$CAUDAL_LIQUIDO.m3.s.))

newx <- seq(-3, 3, by=0.01)

#read regressed data
regression.cantaclaro <- read.csv('Cantaclaro_Data_After_Regression.csv',header = TRUE)

# Save regression
png(filename="LinearModelCantaclaro.png", units = 'cm', width=(21.5),height=(27.9), res = 600)
#par(mar = c(4,4,3,2))
par(mfrow=c(3,2))

plot(log(rating.cantaclaro$CAUDAL_LIQUIDO.m3.s.),log(rating.cantaclaro$GASTO_SOLIDO.Kg.s.),type = 'p',xlab = 'log(water discharge [m3/s])',ylab = 'log(sediment load [kg/s])',pch =21,bg = 'gray',col = 'black')
title("A. Cantaclaro station", adj = 0, line = 0.3)

abline(model1, col="#1b9e77")
graphtext = paste(c('R2 = ',toString(round(summary(model1)$r.squared,2))), collapse = '')
text(x =-1.5,y =-2, graphtext)

conf_interval <- predict(model1, newdata=data.frame(X =newx), interval="confidence",
                         level = 0.95)

lines(newx, conf_interval[,2], col="#7570b3", lty=2)
lines(newx, conf_interval[,3], col="#7570b3", lty=2)

pred_interval <- predict(model1, newdata=data.frame(X=newx), interval="prediction",
                         level = 0.95)
lines(newx, pred_interval[,2], col="#d95f02", lty=3)
lines(newx, pred_interval[,3], col="#d95f02", lty=3)

legend(0, -5.5, legend= c('regression line','95% confidence interval', '95% prediction interval'),
       col=c('#1b9e77','#7570b3','#d95f02'), lty=c(1,2,3), cex=0.8)

plot(as.Date(regression.cantaclaro$date), regression.cantaclaro$new_sedyield, type = 'l',xlab = 'date',ylab = 'sediment load [kg/s]',col = 'black')

dev.off()
