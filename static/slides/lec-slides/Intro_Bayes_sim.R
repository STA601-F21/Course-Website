
######## Simulation Exercise
rm(list = ls())
PlotPriorPlusPosterior <- function(a,b,ones,zeros){
  curve(dbeta(x,a+ones,b+zeros),col="green4",xlab=expression(theta),ylab="density",lwd=1,ylim=c(0,20))
  curve(dbeta(x,a,b),col="red3",add=TRUE,lwd=1)
  legend("topright", legend=c(expression(paste(pi,"(", phi, "|",x,")")),expression(paste(pi,"(", phi,")"))),
         col=c("green4", "red3"), lwd=2, cex=1)
  
  post_mean <- (a+ones)/(a+ones+b+zeros)
  p <- c(0.4, 5)
  points(t(p), pch=16)
  text(t(p), eval(expression(paste("Post. Mean = ",round(post_mean,3)))), adj=-0.05)
}
#PlotPriorPlusPosterior(a=1,b=1,ones=5,zeros=18)

n <- 20
#theta_true <- 0.25
#y <- rbinom(n,1,theta_true)
y <- rep(0,n)
mean(y)

ones <- 0; zeros <- 0
for(i in 1:n){
  if(y[i]==1){
    ones <- ones + 1
  } else {zeros <- zeros + 1}
  PlotPriorPlusPosterior(a=3,b=32,ones,zeros); Sys.sleep(1.5)
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


