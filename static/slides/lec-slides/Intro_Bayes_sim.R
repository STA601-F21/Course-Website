
######## Simulation Exercise
PlotPriorPlusPosterior <- function(a,b,ones,zeros){
  curve(dbeta(x,a+ones,b+zeros),col="blue3",xlab=expression(theta),ylab="density",lwd=2)
  curve(dbeta(x,a,b),col="red3",add=TRUE,lwd=2,lty=2)
  legend("topright", legend=c(expression(paste(pi,"(", phi, "|",x,")")),expression(paste(pi,"(", phi,")"))),
         col=c("blue3", "red3"), lty=1:2,lwd=2, cex=1)
}
#PlotPriorPlusPosterior(a=1,b=1,ones=5,zeros=18)

n <- 30
theta_true <- 0.25
y <- rbinom(n,1,theta_true)
mean(y)

ones <- 0; zeros <- 0
for(i in 1:n){
  if(y[i]==1){
    ones <- ones + 1
  } else {zeros <- zeros + 1}
  PlotPriorPlusPosterior(a=1,b=1,ones,zeros); Sys.sleep(1)
}
