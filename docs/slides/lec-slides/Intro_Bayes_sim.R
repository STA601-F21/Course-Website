
######## Simulation Exercise
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




