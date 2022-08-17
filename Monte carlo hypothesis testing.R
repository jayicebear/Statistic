set.seed(440)

# Set sample size of the dataset
n<-50

# Set true value of 'p'
p<-1/3

# Set 'p' under H0
p0<-1/2

# Observe data (n=50) from Bernoulli(1/3)
X<-rbinom(n,size=1,p=p)

# Estimate 'p' from the observed dataset
phat<-mean(X)

# Simulate data (n=50) from Bernoulli(1/2) for 10,000 times
reps<-10000
X_sim<-matrix(rbinom(n*reps,size=1,p=p0),nrow=reps)

# Estimate 'p' from each of 10,000 simulated datasets
phat_sim<-rowMeans(X_sim)

# Visualize simulations
hist(phat_sim,prob=TRUE,breaks=30)
dens_est<-density(phat_sim, bw="nrd")
points(dens_est$x,dens_est$y,type="l",col="blue")
abline(v=phat,col="red",lty=2)

# Convert test statistics to better handle "both tails"
test_stat<-(phat-p0)^2
test_sim<-(phat_sim-p0)^2

# Visualize simulations
hist(test_sim,prob=TRUE,breaks=30)
abline(v=test_stat,col="red",lty=2)

# Compute a p-value based on simulations
mean(test_sim >= test_stat)

prop.test(sum(X),n=n,p=p0,alternative="two.sided")

t.test(X,mu=p0,alternative="two.sided")

binom.test(sum(X),n=n,p=p0,alternative="two.sided")

#one sample mean test

set.seed(440)

# Set true values of 'mu' and 'sigma'
mu<-3.5; sigma<-5

# Set the value of 'mu' under H0
mu0<-3

# Set the sample size of the dataset
n<-100

# Set the number of simulations to run
reps<-1000

# Observe data (n=100) from N(3.5, 5^2)
X<-rnorm(n=n,mean=mu,sd=sigma)

# Estimate 'mu' from the observed dataset
muhat<-mean(X)

# Simulate data (n=100) from N(3, 5^2) for 1000 times
X_sim<-matrix(rnorm(n=n*reps,mean=mu0,sd=sigma),nrow=reps) # Estimate 'mu' from each of 1000 simulated datasets
muhat_sim<-rowMeans(X_sim)

# Visualize simulations
hist(muhat_sim,prob=TRUE,breaks=30)
dens_est<-density(muhat_sim)
points(dens_est$x,dens_est$y,type="l",col="blue")
abline(v=muhat,col="red",lty=2)

 # Compute two-sided p-value based on simulations
mean((muhat_sim-mu0)^2>=(muhat-mu0)^2)

t.test(X,mu=mu0,alternative="two.sided")

wilcox.test(X,mu=mu0,alternative="two.sided")

