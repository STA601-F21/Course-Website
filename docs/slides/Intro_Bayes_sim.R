
######## Simulation Exercise
rm(list = ls())
PlotPriorPlusPosterior <- function(a,b,ones,zeros){
  curve(dbeta(x,a+ones,b+zeros),col="green4",xlab=expression(theta),ylab="density",lwd=1,ylim=c(0,15))
  curve(dbeta(x,a,b),col="red3",add=TRUE,lwd=1)
  legend("topright", legend=c(expression(paste(pi,"(", phi, "|",x,")")),expression(paste(pi,"(", phi,")"))),
         col=c("green4", "red3"), lwd=2, cex=1)
  
  post_mean <- (a+ones)/(a+ones+b+zeros)
  p <- c(0.75, 7)
  points(t(p), pch=16)
  text(t(p), eval(expression(paste("Post. Mean = ",round(post_mean,3)))), adj=-0.05)
}
#PlotPriorPlusPosterior(a=1,b=1,ones=5,zeros=18)

set.seed(360602)
n <- 300
theta_true <- 0.5
y <- rbinom(n,1,theta_true)
#y <- rep(0,n)
mean(y)

ones <- 0; zeros <- 0
for(i in 1:n){
  if(y[i]==1){
    ones <- ones + 1
  } else {zeros <- zeros + 1}
  PlotPriorPlusPosterior(a=1,b=1,ones,zeros)
  points(t(c(0.75, 8)), pch=16)
  text(t(c(0.75, 8)), paste("Observation",i," = ",y[i]), adj=-0.05)
  if(i < 8){
    Sys.sleep(10)
  }
  if(i > 8 & i < 25){
    Sys.sleep(2)
  }
  if(i > 25){
    Sys.sleep(0.05)
  }
}


#####
Data <- read.csv("data/lalondedata.txt",header=T)
Data$Diff <- Data$re78 - Data$re74
head(Data)
summary(Data)
round(by(Data[,c("Diff")],Data[,"treat"], mean),2)
round(by(Data[,c("Diff")],Data[,"treat"], sd),2)

plot(density(Data$Diff[Data$treat==0]),lwd=1.5,type="l",
     col="green4",xlab="Diff wages",ylab="Density",main="Change in real annual earnings for the two groups")
lines(density(Data$Diff[Data$treat==1]),col="orange3",lwd=1.5,type="l")
legend("topright",legend=c("did not receive job training","received job training"),
       col=c("green4","orange3"), lwd=2, cex=1)


hist(Data$Diff[Data$treat==0],col="green4",xlab="Diff wages",
     main="Change in real annual earnings for the two groups")
hist(Data$Diff[Data$treat==1],col="orange3",add=T)
legend("topright",legend=c("did not receive job training","received job training"),
       col=c("green4","orange3"), lwd=2, cex=1)


####
u_star <- runif(1000,pbeta(0.4,5,10),pbeta(0.75,5,10))
theta <- qbeta(u_star,5,10)
mean(theta)
var(theta)

#0.47|0.49
#0.003|0.005

###
n = 1000
while((1.96*((sd(rgamma(n,68,45)))/sqrt(n)))>0.001){
  n = n+10
}
n









#################
library(MASS)
library(mvtnorm)
library(lattice)

rho <- 0.95
Sigma <- matrix(c(1,rho,rho,1),ncol=2)
Mu <- c(0,0)


x.points <- seq(-3,3,length.out=100)
y.points <- x.points
z <- matrix(0,nrow=100,ncol=100)

for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x.points[i],y.points[j]),mean=Mu,sigma=Sigma)
  } 
}

#wireframe(z~x+y,data=data.frame(x=x.points,y=rep(y.points, each=length(x.points)), z=z),
#          xlab=expression(theta[1]),ylab=expression(theta[2]),zlab="Density",
#          col="orange4",main=expression(paste(rho," = 0")))
#sampmat_direct <- rmvnorm(100000, mean = Mu,sigma = Sigma)
#sampmat_direct.kde <- kde2d(sampmat_direct[,1], sampmat_direct[,2], n = 50)
#image(sampmat_direct.kde,main=expression(paste(rho," = 0")))
#contour(sampmat_direct.kde, add = T)

contour(x.points,y.points,z,xlim=c(-3,10),ylim=c(-3,10),col="orange2",
        xlab=expression(theta[1]),ylab=expression(theta[2]))
S <- 50
sampmat <- matrix(0,nrow=S,ncol=2)
samp <- c(10,10)
points(x=samp[1],y=samp[2],col="black",pch=2)
for (s in 1:S) {
  samp[1] <- rnorm(1,rho*samp[2],sqrt(1-rho^2))
  samp[2] <- rnorm(1,rho*samp[1],sqrt(1-rho^2))
  sampmat[s,] <- samp
  if(s < 20){
    points(x=samp[1],y=samp[2],col="red4",pch=16)
    Sys.sleep(1)
  } else {
    points(x=samp[1],y=samp[2],col="green4",pch=16)
    Sys.sleep(0.1)
  }
}
#plot(sampmat)







########
#install.packages(c("coda","mvtnorm","devtools","loo","dagitty"))
#library(devtools)
#devtools::install_github("rmcelreath/rethinking",ref="Experimental")

library(rethinking)
data(Howell1)
dim(Howell1)
head(Howell1)

sum(Howell1$age < 15 & Howell1$male==1)
sum(Howell1$age < 15 & Howell1$male==0)

sum(Howell1$age > 75 & Howell1$male==1)
sum(Howell1$age > 75 & Howell1$male==0)



