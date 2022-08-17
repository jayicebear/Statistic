 target = function(x){
  return(ifelse(x<0,0,exp(-x)))
}

set.seed(440)
x = rep(0,10000)
x[1] = 3 #initialize MH: I've set arbitrarily set this to 3 for(i in 2:10000){
  current_x = x[i-1]
  proposed_x = current_x + rnorm(1,mean=0,sd=1)
  A = target(proposed_x)/target(current_x)
  if(runif(1)<A){
x[i] = proposed_x # accept move with probability min(1,A)
} else {
x[i] = current_x # otherwise "reject" move, and stay where we are
}
}

plot(x,pch=20,col="grey",main="Values of x visited by the MH algorithm")

hist(x,xlim=c(0,10),probability = TRUE, main="Histogram of values of x visited by MH algorithm")
xx = seq(0,10,length=100)
lines(xx,target(xx),col="red")

mu<-0
sig<-0.3
mc_N<-10000
X<-numeric(mc_N)
X[1]<-0
f<-function(x){dnorm(x,mean=mu,sd=sig)}
Q<-function(x1,x2){dunif(x1,min=x2-1,max=x2+1)}
accept_fun<-function(x_c,x_p){
  accept<-f(x_p)*Q(x_c,x_p)/(f(x_c)*Q(x_p,x_c))
  return(min(accept,1))
}
for(i in 2:mc_N){
 x_prop<-X[i-1]+runif(1,min=-1,max=1)
  accept<-accept_fun(X[i-1],x_prop)
  dec<-rbinom(1,1,accept)
  if(dec==1){
    X[i] = x_prop
  }else{
    X[i] = X[i-1]
  }
}

hist(X,probability = TRUE, main="Histogram of values of x visited by MH algorithm")
curve(dnorm(x,mean=mu,sd=sig),add=TRUE,col="red")

