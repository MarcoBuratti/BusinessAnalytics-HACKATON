library("readxl")
library(rgl)
library(kknn)

# data <- read_excel('Guns.xls')

data <- Guns
names(data)
sapply(data, class)


#response
violent <- log(data$violent) 
#sapply(violent, class)
#regressor
afam <- data$afam

# Plot data with the log value on y axis
plot(afam, violent, xlim=c(0,15), ylim=c(3,8)) 
grid()
abline(0,1, lty=3)
# Fit a linear model
fm <- lm(violent ~ afam)
summary(fm)
# plot the regression line and the fitted values
abline(coefficients(fm), col='red')
points(afam, fitted(fm), col='red', pch=16)
# Plot the avg value of afam on x-axis and violent on y-axis
abline(h=mean(violent))
abline(v=mean(afam))



# Create a qq-plot of the residuals
qqnorm(residuals(fm))
# Perform a Normality test of the residuals
shapiro.test(residuals(fm))
# Print the estimates of the regression coefficients
coefficients(fm)
# Compute CI of the regression coefficients
confint(fm, level= 0.95)
# Print the estimate of the error variance
s2 <- sum(residuals(fm)^2)/fm$df
sqrt(s2)

# Compute a set of CIs
# Build a new data base
Z0   <- data.frame(cbind(afam=seq(0, 35, by=0.1)))
# Compute the CIs
CI <- predict(fm, Z0, interval='confidence')
# Plot the CIs
plot(afam, violent, xlim=c(0,35), ylim=c(4,8))
lines(Z0[,1], CI[,'fit'])
lines(Z0[,1], CI[,'lwr'], lty=4)
lines(Z0[,1], CI[,'upr'], lty=4)
# Compute a set of PIs
PI <- predict(fm, Z0, interval='prediction', level=0.95)
# Plot the PIs
lines(Z0[,1], PI[,'fit'])
lines(Z0[,1], PI[,'lwr'], lty=2)
lines(Z0[,1], PI[,'upr'], lty=2)


# Import the second regressor
population <- data$population
# Plot data with the log value on y axis
plot(population, violent, xlim=c(0,15), ylim=c(3,8)) 
grid()
abline(0,1, lty=3)
# Fit a linear model
fm2 <- lm(violent ~ population)
summary(fm2)
# plot the regression line and the fitted values
abline(coefficients(fm2), col='red')
points(population, fitted(fm2), col='red', pch=16)


# Add a third regressor
income <- data$income
# Plot data with the log value on y axis
plot(income, violent, xlim=c(9000,20000), ylim=c(3,8)) 
grid()
abline(0,1, lty=3)
# Fit a linear model
fm3 <- lm(violent ~ income)
summary(fm3)
# plot the regression line and the fitted values
abline(coefficients(fm3), col='red')
points(income, fitted(fm3), col='red', pch=16)


fm3 <- lm(violent ~ afam + population + income)
summary(fm3)
pairs(cbind(violent, afam, population, income))

#Plot in 3D pop, afam and violent
open3d()
plot3d(x=afam, y=population, z=violent, size=10, col='black')
# Fit the linear model
fm2 <- lm(violent ~ afam + population)
summary(fm2)
# plot the regression surface and the fitted values
points3d(x=afam, y=population, z=fitted(fm2), size=10, col='red')
planes3d(coefficients(fm2)[2],coefficients(fm2)[3],-1,coefficients(fm2)[1], alpha=0.5, color='red')


# Plot data with the log value on y axis
plot(afam, violent, xlim=c(0,15), ylim=c(3,8)) 
grid()
abline(0,1, lty=3)
# Fit a linear model
fm <- lm(violent ~ afam)
summary(fm)
# plot the regression line and the fitted values
abline(coefficients(fm), col='red')
# Compute the fitted values using k=8 and the rectangular kernel
Z0   <- data.frame(cbind(afam=seq(0, 35, by=0.1)))
predicted.response <- kknn(violent ~ afam, 
                           train = data.frame(afam, violent),
                           test = Z0, 
                           k = 18, kernel='rectangular')
# Plot the knn prediction line
lines(Z0[,'afam'], predicted.response$fitted.values, col='blue')
# Select the value of k (leave-one-out crossvalidation method)
train.cv <- train.kknn(violent ~ afam, data = data.frame(afam, violent), 
                       kmax = 40, scale = F, kernel = 'rectangular')
plot(train.cv)
grid()

