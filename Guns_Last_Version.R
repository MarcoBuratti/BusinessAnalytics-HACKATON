library(rgl)
library(resample)
library(plotrix)
library(cluster)

data <- Guns[1:9]
names(data)
sapply(data, class)

#data$prisoners <- as.numeric(as.integer(data$prisoners))       
#data$law <- as.numeric(as.integer(data$law)) 
#data$murder <- NULL
#data$robbery <- NULL

#Standardize Date from [2:9], first element is the State
standardize <- function(x) {
  return ((x - mean(x)) / sd(x)) }
data.standard <- as.data.frame(lapply(data[2:9], standardize))

open3d()
plot3d(data.standard, size=3, col='orange', aspect = F)
axes3d()

#Matrix of Covariance
data.cov <- cov(data.standard)

### PCA: Build the PCx vector from standard Dataset 
pca.Data <- princomp(data.standard, scores=T)
summary(pca.Data)

### THIS GRAPH DO NOT HAVE MUCH SENSE
#barplot(diag(var(data.cov)), las=2, main='Original features', ylim=c(0,0.4), ylab='Variances')

# make a barplot of the variances of the three PCs (explained total variance)
barplot(pca.Data$sdev, las=2, main='Variance of PCx', ylim=c(0,2), ylab='Variances')

# make a barplot of the cumulated variances of the three PCs (cumulated explained total variance)
barplot(cumsum(pca.Data$sdev^2), las=2, main='Cumulated var of PCx', ylim=c(0,10), ylab='Variances')


# Compute the loadings
load.Data    <-  pca.Data$loadings #pc.Q$loadings
load.Data

# Make a barplot for each of the three loading vectors
par(mfrow = c(4,1))
for(i in 1:8)
{
  barplot(load.Data[,i], main='Loading Vector of PC', ylim = c(-1, 1))
  abline(h=0)
}

# Compute the scores
scores.Data <- pca.Data$scores
scores.Data

boxplot(data.standard, las=2, col='red', main='Original features')
# How many principal components? Fraction of explained total variance
plot(0:dim(data.standard)[2],c(0, cumsum(pca.Data$sdev^2)/sum(pca.Data$sde^2)), type='b', xlab='Nr of Pcs', ylab='Cumulated fraction of total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')

# Make boxplots of the scores of the three PCs
boxplot(scores.Data, las=2, col='red', main='Principal components')


media <- colMeans(data.standard)
#### PLOT WITH WINDOWS COMPUTER
projection <- matrix(media, dim(data.standard)[[1]], dim(data.standard)[[2]], byrow=T)
points3d(projection, size=8)

projection <- projection + scores.Data[,1] %*% t(load.Data[,1])
points3d(projection, size=3, col='red')

projection <- projection + scores.Data[,2] %*% t(load.Data[,2])
points3d(projection, size=3, col='blue')

projection <- projection + scores.Data[,3] %*% t(load.Data[,3])
points3d(projection, size=3, col='green')

# Plot and interpret the three PCs on eby one
par(mfrow=c(3,1))
for(i in 1:3)
{
  plot(cbind(scores.Data[,i], 0), asp=1, pch=16, col='orange', 
       xlim=c(-5,5), main=paste('PC',i), ylab='', xlab='Scores')
  abline(v=0, lty=2)
  arrows(0,0,load.Data[,i]*sqrt(sum(diag(var(data.standard)))), 0, col='blue', pch=16, lwd=2)
  text(cbind(load.Data[,i]*sqrt(sum(diag(var(data.standard)))), 0), labels = dimnames(data.standard)[[2]], col='blue', pos=1)
}

# Plot and interpret the first two PCs jointly
par(mfrow=c(1,1))
plot(scores.Data[,1:2], asp=1, col='orange', pch=16)
# To insert name of each state into graph delete '#'
text(scores.Data[,1],scores.Data[,2], data$State , cex=0.5)
abline(h=0, lty=2)
abline(v=0, lty=2)
draw.circle(0,0,sqrt(sum(diag(var(data.standard)))), lty=2)
arrows(0,0,load.Data[,1]*sqrt(sum(diag(var(data.standard)))), load.Data[,2]*sqrt(sum(diag(var(data.standard)))), col='blue', pch=16, lwd=2)
text(load.Data[,1:2]*sqrt(sum(diag(var(data.standard)))), labels = dimnames(data.standard)[[2]], col='blue')

plot(scores.Data[,1],scores.Data[,2],type="n",xlab="pc1",ylab="pc2", asp=1)
text(scores.Data[,1],scores.Data[,2], data$State , cex=0.5)
abline(h=0, lty=2)
abline(v=0, lty=2)
draw.circle(0,0,sqrt(sum(diag(var(data.standard)))), lty=2)
arrows(0,0,load.Data[,1]*sqrt(sum(diag(var(data.standard)))), load.Data[,2]*sqrt(sum(diag(var(data.standard)))), col='blue', pch=16, lwd=2)
text(load.Data[,1:2]*sqrt(sum(diag(var(data.standard)))), labels = dimnames(data.standard)[[2]], col='blue')


# Project data points on the space spanned by the k-th PC
par(mfrow=c(2,4))
for(i in 1:3)
{
  projection <- matrix(media, dim(data.standard)[[1]], dim(data.standard)[[2]], byrow=T) + scores.Data[,i] %*% t(load.Data[,i])
  matplot(t(projection), type='l', main = paste(i, 'PC'), ylim=range(data.standard))
  matplot(media, type='l', lwd=2, add=T)
}

###### CLUSTER
Guns_0
Guns_1

data_0 <- Guns_0[1:9]
data_1 <- Guns_1[1:9]


#Standardize Date from [2:9], first element is the State
standardize <- function(x) {
  return ((x - mean(x)) / sd(x)) }
data.standard_0 <- as.data.frame(lapply(data_0[2:9], standardize))

pca.Data_0 <- princomp(data.standard_0, scores=T)

scores.Data_0 <- pca.Data_0$scores

kmc <- kmeans(data.standard_0, 5)
#kmc$cluster
#kmc$centers
#kmc$size

clusplot(data.standard_0, kmc$cluster, main='Two component representation of the Cluster set', 
                        color=TRUE, shade=TRUE, labels=1, lines=0)
text(scores.Data_0[,1],scores.Data_0[,2], data_0$State , cex=1)

#####Standardize Date from [2:9], first element is the State
data.standard_1 <- as.data.frame(lapply(data_1[2:9], standardize))
pca.Data_1 <- princomp(data.standard_1, scores=T)
scores.Data_1 <- pca.Data_1$scores

kmc <- kmeans(data.standard_1, 5)
kmc$cluster
kmc$centers
kmc$size

clusplot(data.standard_1, kmc$cluster, main='Two component representation of the Cluster set', 
         color=TRUE, shade=TRUE, labels=2, lines=0)
text(scores.Data_1[,1],scores.Data_1[,2], data_1$State , cex=1)


### CLUSTER WITH BOTH 0 ADN 1
kmc <- kmeans(data.standard, 5)
kmc$cluster
kmc$centers
kmc$size
clusplot(data.standard, kmc$cluster, main='Two component representation of the Cluster set', 
         color=TRUE, shade=TRUE, labels=2, lines=0)
text(scores.Data[,1],scores.Data[,2], data$State , cex=1)


