library(ggplot2)
library(gridExtra)
# define x range for plotting
xs = seq(.001, .999, .001)
# generate Bernoulli samples
set.seed(440)
x_samps = rbinom(1000, 1, .7)
plot_posterior = function(n, prior_a, prior_b) { #'
  #' @param n sample size
  #' @param prior_a prior alpha value
  #' @param prior_b prior beta value
  #'
  # calculate sample sum
  x_sum = sum(x_samps[1:n])
  # calculate alpha and beta values
  post_a = prior_a + x_sum
  post_b = prior_b + n - x_sum
# create plot
  ggplot(data=data.frame(x=xs, y=dbeta(xs, post_a, post_b)),
         mapping=aes(x=x, y=y)) +
    geom_line() +
    geom_vline(xintercept=qbeta(c(.025, .975), post_a, post_b)),
               color="blue", linetype="dashed") +
    geom_vline(xintercept=.7,
               color="red",
               linetype="dashed") +
    ggtitle(sprintf("n = %s, alpha = %s, beta = %s", n, prior_a, prior_b)) +
    xlab("Theta") +
    ylab("Density")
}
p1 = plot_posterior(0, 2, 2)
p2 = plot_posterior(10, 2, 2)
p3 = plot_posterior(100, 2, 2)
grid.arrange(p1, p2, p3, ncol=1)

# larger sample size
p1 = plot_posterior(0, 100, 300)
p2 = plot_posterior(10, 100, 300)
p3 = plot_posterior(100, 100, 300)
grid.arrange(p1, p2, p3, ncol=1)


# uniform prior: beta(1, 1)
unif_prior_est = ggplot(data=data.frame(x=xs,
                       y=dbeta(xs, 71, 31)),
         mapping=aes(x=x, y=y)) +
    geom_line() +
    geom_vline(xintercept=qbeta(c(.025, .975), 71, 31),
               color="blue", linetype="dashed") +
    ggtitle(sprintf("Vaccination rate estimate with n = %s,alpha = %s, beta = %s", 100, 1, 1)) +
    xlab("Posterior vaccination rate estimate") +
    ylab("Density")
    
    
# inference vacination rate
# Centre county prior: beta(63, 37)
centre_prior_est = ggplot(data=data.frame(x=xs, y=dbeta(xs,133,67)),
         mapping=aes(x=x, y=y)) +
    geom_line() +
    geom_vline(xintercept=qbeta(c(.025, .975), 133, 67),
               color="blue", linetype="dashed") +
    ggtitle(sprintf("n = %s, alpha = %s, beta = %s", 100, 63,37)) + 
    xlab("Posterior vaccination rate estimate") +
    ylab("Density")
grid.arrange(unif_prior_est, centre_prior_est, ncol=1)

