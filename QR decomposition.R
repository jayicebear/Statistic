n <- 50;M <- 500
x <- seq(1,M,len=n)
X <- cbind(1,x,xˆ2,xˆ3)
colnames(X) <- c("Intercept","x","x2","x3")

beta <- matrix(c(1,1,1,1),4,1)
set.seed(440)
y <- X%*%beta+rnorm(n,sd=1)

solve(crossprod(X)) %*% crossprod(X,y)

crossprod(X)

log10(crossprod(X))

# LES computation with QR decomposition

QR <- qr(X)
Q <- qr.Q( QR )
R <- qr.R( QR )
betahat <- backsolve(R, crossprod(Q,y) )
print(betahat)

QR <- qr(X)
betahat <- solve.qr(QR, y)
print(betahat)

summary(lm(y~0+X))$coef[,1]
