library(ggplot2)
bivariate_gibbs = function(m, x0,
                           mu1, mu2,
                           sigma1, sigma2,
                           rho) {
  #'
  #' @param m chain length
  #' @param x0 initial state
  #' @param mu1 d1 mean
  #' @param mu2 d2 mean
  #' @param sigma1 d1 standard deviation
  #' @param sigma2 d2 standard deviation
  #' @param rho correlation
  #' initialize matrix of results
  results = matrix(0, nrow=m, ncol=2)
  results[1, ] = x0
for (i in 1:(m-1)) {
# update first element of current state results[i+1, 1] = rnorm(
      1,
      mu1 + rho * sigma1 / sigma2 * (results[i, 2] - mu2),
      sqrt((1 - rho**2) * sigma1**2)
)

# update second element of current state
    results[i+1, 2] = rnorm(
      1,
      mu2 + rho * sigma2 / sigma1 * (results[i+1, 1] - mu1),
      sqrt((1 - rho**2) * sigma2**2)
    )
}
  # return results
  data.frame(results)
}

sim_gibbs_res = bivariate_gibbs(
  10000, c(0, 0),
  1, 1,
  sqrt(2), sqrt(2),
-.5 )
ggplot(data=sim_gibbs_res,
       mapping=aes(x=X1, y=X2)) +
  geom_bin2d() +
  geom_density_2d(color="white") +
  ggtitle("Gibbs sampling histogram")
  
library(MASS)
sim_naive_res = data.frame(
  mvrnorm(10000,
          c(1, 1),
          matrix(c(2, -1, -1, 2), nrow=2))
)
ggplot(data=sim_naive_res,
       mapping=aes(x=X1, y=X2)) +
  geom_bin2d() +
  geom_density_2d(color="white") +
  ggtitle("Naive MC sampling histogram")
  
# Galton Height data
  
library(HistData)
# calculate Galton parameters
g_mu1 = mean(Galton$parent)
g_mu2 = mean(Galton$child)
g_s1 = sd(Galton$parent)
g_s2 = sd(Galton$child)
g_rho = cor(Galton$parent, Galton$child)
set.seed(440)
gibbs_res = bivariate_gibbs(10000, c(g_mu1, g_mu2),
                            g_mu1, g_mu2,
                            g_s1, g_s2,
                            g_rho)

names(gibbs_res) = c("ParentHeight", "ChildHeight")
ggplot(data=gibbs_res, mapping=aes(x=ParentHeight,
                                   y=ChildHeight)) +
  geom_bin2d() +
  geom_density_2d(color="white") +
  ggtitle("Gibbs sampling histogram")  

naive_mc_res = data.frame(mvrnorm(10000,
                                  colMeans(Galton),
                                  cov(Galton)))
names(naive_mc_res) = c("ParentHeight", "ChildHeight")
  
ggplot(data=naive_mc_res, mapping=aes(x=ParentHeight,
                                      y=ChildHeight)) +
  geom_bin2d() +
  geom_density_2d(color="white") +
  ggtitle("Naive MC sampling histogram")

res2 = bivariate_gibbs(10000, c(71, 107),
                       g_mu1, g_mu2,
g_s1, g_s2,
                       g_rho)
names(res2) = c("ParentHeight", "ChildHeight")
ggplot(data=res2, mapping=aes(x=ParentHeight,
                              y=ChildHeight)) +
  geom_bin2d() +
  geom_density_2d(color="white")
  
  
  
