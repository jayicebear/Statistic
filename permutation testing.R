set.seed(123)
sample(1:5)

same_dist_perm_test = function(
  n_perms, xs, ys, test_statistic
){
  #' perform a generic permutation test
  #' @param n_perms number of permutations to generate
  #' @param xs vector of samples from distribution X
  #' @param yx vector of samples from distribution y
  #' @param test_statistic function that calculates the test statistic
  # calculate the number of samples in X and Y
n = length(xs)
m = length(ys)
# define labels (1 = X samples, 0 = y samples) labels = c(rep(1, n), rep(0, m))
all_data = c(xs, ys)
  # for every permutation replication
  replicate(
    n_perms, {
      # permute label orders
      permuted_labels = sample(labels)
      # generate new test statistic under permutation
      test_statistic(all_data[permuted_labels == 1],
                     all_data[permuted_labels == 0])
}
) }

set.seed(123)
# create example data
x_obs = rnorm(100)
y_obs = rnorm(100)
# generate permutation samples of T
obs_perms = same_dist_perm_test(
  1000,
  
   x_obs,
  y_obs,
  function(a, b) { mean(a) - mean(b) }
)
# plot histogram of T
library(ggplot2)
ggplot(data=data.frame(x=obs_perms)) +
  geom_histogram(aes(x=x, y=..density..), bins=30,
                 color="black", fill="white") +
  geom_vline(xintercept=mean(x_obs) - mean(y_obs),
             color="blue", size=1.5) +
  xlab("Difference in sample means") +
  ylab("Density")
  
  # calculate and return the p-value
p_value = mean(obs_perms <= (mean(x_obs) - mean(y_obs)))
p_value
  
set.seed(1234)
# replication experiment: for each replication...
p_value_replicates = replicate(
  1000,
{
# sample the observed data
x_obs = rnorm(100)
y_obs = rnorm(100)
# perform the permutation test
obs_perms = same_dist_perm_test(
  100,
  x_obs,
  y_obs,
  function(a, b) { mean(a) - mean(b) }
)
    # calculate and return the p-value
    mean(obs_perms <= (mean(x_obs) - mean(y_obs)))
  }
)
# plot results
ggplot(data=data.frame(x=p_value_replicates)) +
  geom_histogram(aes(x=x, y=..density..), bins=20,
                 color="black", fill="white") +
  geom_line(data=data.frame(x=seq(-.1, 1.1, .01),
                            y=dunif(seq(-.1, 1.1, .01))),
            mapping=aes(x=x,y=y),
            linetype="dashed",
            color="blue") +
  xlab("Simulated p-values") +
  ylab("Density")
  
set.seed(123)
# generate samples
x_obs = rnorm(1000, sd=sqrt(2))
y_obs = rexp(1000, 1) - rexp(100, 1)
# plot samples
ggplot(data=data.frame(x=c(x_obs, y_obs),
                       lab=c(rep("Normal(0, 2)", 100),
                             rep("Laplace(0, 1)", 100)))) +
  geom_histogram(aes(x=x, y=..density..),
                 bins=30, color="black", fill="white") +
  facet_grid(lab ~ .)

obs_perms = same_dist_perm_test(
  10000,
  x_obs,
  y_obs,
  function(a, b) {
    (max(a) - min(a)) -
      (max(b) - min(b))
} )
observed_test_stat = (
  max(x_obs) - min(x_obs) -
    (max(y_obs) - min(y_obs))
)
ggplot(data=data.frame(x=obs_perms)) +
  geom_histogram(aes(x=x, y=..density..),
                 bins=30, color="black", fill="white") +
  geom_vline(xintercept=(max(x_obs) - min(x_obs) -
                         (max(y_obs) - min(y_obs))),
           color="blue",
           size=1.5) +
xlab("Distribution of Range(X) - Range(Y)") +
ylab("Density")

mean(observed_test_stat >= obs_perms)

  
  

