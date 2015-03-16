
# Bootstrap 95% CI for "analytical target"
library(boot)
# function to obtain "analytical target" from the data

# rsq <- function(formula, data, indices) {
#   d <- data[indices,] # allows boot to select sample 
#   fit <- lm(formula, data=d)
#   return(summary(fit)$r.square)
# }

# bootstrapping with 1000 replications 
# results <- boot(data=mtcars, statistic=rsq, R=1000, formula=mpg~wt+disp)

# function to obtain "analytical target" from the data
boot.max <- function(data, indices)
{
  d <- data[indices, ]
  return(c(max(d$d1), max(d$d2)))
}

boot.sum <- function(data, indices)
{
  d <- data[indices, ]
  return(c(sum(d$d1), sum(d$d2)))
}

a <- rnorm(1000, 0, 1)
b <- rnorm(1000, 0, 0.5)

df <- data.frame('d1' = a, 'd2' = b)

# bootstrapping with 1000 replications 
results <- boot(data=df, statistic=boot.sum, R=1000)

head(results$t)
summary(results$t)

# view results
results 
plot(results)

# get 95% confidence interval 
boot.ci(results, type="bca")

# get 95% confidence interval 
boot.ci(results, type = "bca", index = 1)
boot.ci(results, type = "bca", index = 2)

bootplot <- function(b, index, main = "") {
  dens <- density(b$t[, index])
  ci <- boot.ci(b, type = "bca", index = index)$bca[4:5]
  est <- b$t0[index]
  plot(dens, main = main)
  y <- max(dens$y)/10
  segments(ci[1], y, ci[2], y, lty = 2)
  points(ci[1], y, pch = "(")
  points(ci[2], y, pch = ")")
  points(est, y, pch = 19)
}

layout(matrix(1:2, ncol = 2))
bootplot(results, 1, main = expression(mu[1]))
bootplot(results, 2, main = expression(mu[2]))


r.plot <- melt(results$t)

ggplot(r.plot, aes(value, fill = factor(Var2))) + geom_histogram(alpha = 0.6, aes(y=..density..), binwidth=5) + geom_density(alpha=.2, aes(linetype=factor(Var2)), size=1) + geom_vline(aes(xintercept=mean(results$t[, 1])), color="red", size=1, linetype="dashed", alpha = 0.6) + geom_vline(aes(xintercept=mean(results$t[, 2])), color="blue", size=1, linetype="solid", alpha = 0.6)
