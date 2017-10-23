#PART 1 

data = read.table("gauge.txt")
data1 = (as.matrix(data[2:91,]))
class(data1) = "numeric"
gain_transformed = log(data1[,2])
density = data1[,1]
summary(fit1<- lm(gain_transformed~density))


png("Gain vs density.png")
plot(density, gain_transformed)
#plot(mean(density), mean(gain_transformed))
abline(fit1, col = "blue")
dev.off()

# cbind(gain_transformed, density)
png("residuals.png")
plot(fit1$residuals)
sum(fit1$residuals^2)
lines(x<- c(0:91),matrix(0,92,1), col = "red")
dev.off()

png("hist_residuals.png")
hist(fit1$residuals, breaks = 25, freq = F)
dev.off()

png("NORM_PLOT.png")
qqnorm(fit1$residuals)
qqline(fit1$residuals)
dev.off()

summary(gain_transformed)


#PART 2

newx = sort(unique(density))
#Bootstrap 
mean(fit1$residuals)
B = 400 # the number of bootstrap samples we want
boot.sample <- array(dim = c(B, 80))
boot.gain_transformed <- array(dim = c(B, 80))
coeff <- array(dim = c(B, 2))
X = Y <- array(dim = c(B, 8))
for (i in 1:B) { # one boot iteration
  boot.sample[i, ] <- sample(fit1$residuals, size = 80, replace =TRUE)
boot.gain_transformed[i,]<- boot.sample[i,] + fit1$fitted.values
fit = lm(boot.gain_transformed[i,]~density)
coeff[i,] = fit$coefficients
for (j in 1:9) # calculating 9 Xs in each boot iteration corresponding to 9 mean Ys
 {Y[i,j] = mean(boot.gain_transformed[i,((j-1)*10+1) : (j*10)]) # mean Ys in each iteration
X[i,j] = (Y[i,j] - coeff[i,1])/coeff[i,2] # Point estimate for 9 Ys in each iteration 
 }
}

#Plotting the quantiles 
quant <- function(x) {quantile(x, c(0.025, 0.5, 0.975))}
quant(X[,1])
quantiles <- apply(X,2, quant)

avg_gain = NULL
#Mean gains for 9 densities 
for (j in 1:9)
{avg_gain[j] = mean(gain_transformed[((j-1)*10+1) : (j*10)])
}
png("Calibration.png")
plot(gain_transformed, density, main = "Confidence Intervals", xlab = "log(Gain)", ylab = "Density")
lines( avg_gain,quantiles[1,], col="magenta", lty=2)
lines( avg_gain,quantiles[3,], col="magenta", lty=2)
lines( avg_gain,quantiles[2,], col="blue", lty=1)
dev.off()


#PART 3

#Bootstrap 

new_gain = gain_transformed[ - c(81:90)]
new_density = density [ -c(81:90)]
summary(fit3<- lm(new_gain~new_density))
B = 400 # the number of bootstrap samples we want
boot.sample <- array(dim = c(B, 80))
boot.gain_transformed <- array(dim = c(B, 80))
coeff <- array(dim = c(B, 2))
X = Y <- array(dim = c(B, 8))
temp = NULL
for (i in 1:B) { # one boot iteration
  boot.sample[i, ] <- sample(fit3$residuals, size = 80, replace =TRUE)
  boot.gain_transformed[i,]<- boot.sample[i,] + fit3$fitted.values
  fit = lm(boot.gain_transformed[i,]~new_density)
  coeff[i,] = fit$coefficients
  temp[i] = (6.056036 - coeff[i,1])/coeff[i,2]
  
}

#Plotting the quantiles 
quant <- function(x) {quantile(x, c(0.025, 0.5, 0.975))}
quant(temp)





