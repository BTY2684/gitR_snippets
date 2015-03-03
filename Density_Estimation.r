logL <- function(param, x) {
  d1 <- dnorm(x, mean = param[2], sd = param[3])
  d2 <- dnorm(x, mean = param[4], sd = param[5])
  -sum(log(param[1] * d1 + (1 - param[1]) * d2))
}
startparam <- c(p = 0.5, mu1 = 50, sd1 = 3, mu2 = 80, sd2 = 3)
opp <- optim(startparam, logL, x = faithful$waiting, method = "L-BFGS-B", lower = c(0.01, rep(1, 4)), upper = c(0.99, rep(200, 4)))
opp

data("faithful", package = "datasets")
x <- faithful$waiting
layout(matrix(1:3, ncol = 3))
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency", probability = TRUE, main = "Gaussian kernel", border = "gray")
lines(density(x, width = 12), lwd = 2)
rug(x)
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency", probability = TRUE, main = "Rectangular kernel", border = "gray")
lines(density(x, width = 12, window = "rectangular"), lwd = 2)
rug(x)
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency", probability = TRUE, main = "Triangular kernel", border = "gray")
lines(density(x, width = 12, window = "triangular"), lwd = 2)
rug(x)

pkgImport("KernSmooth")
pkgImport("HSAUR")
data("CYGOB1", package = "HSAUR")
CYGOB1d <- bkde2D(CYGOB1, bandwidth = sapply(CYGOB1, dpik))
contour(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat, xlab = "log surface temperature", ylab = "log light intensity")

persp(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat,xlab = "log surface temperature", ylab = "log light intensity", zlab = "estimated density", theta = -35, axes = TRUE, box = TRUE)

pkgImport("mclust")
mc <- Mclust(faithful$waiting)
mc
mc$parameters$mean
sqrt(mc$parameters$variance$sigmasq)
pkgImport("flexmix")
fl <- flexmix(waiting ~ 1, data = faithful, k = 2)
parameters(fl, component = 1)
parameters(fl, component = 2)

pkgImport("boot")
fit <- function(x, indx) {
  a <- Mclust(x[indx], minG = 2, maxG = 2)$parameters
  if (a$pro[1] < 0.5)
    return(c(p = a$pro[1], mu1 = a$mean[1], mu2 = a$mean[2]))
  return(c(p = 1 - a$pro[1], mu1 = a$mean[2], mu2 = a$mean[1]))
}
opar <- as.list(opp$par)
rx <- seq(from = 40, to = 110, by = 0.1)
d1 <- dnorm(rx, mean = opar$mu1, sd = opar$sd1)
d2 <- dnorm(rx, mean = opar$mu2, sd = opar$sd2)
f <- opar$p * d1 + (1 - opar$p) * d2
hist(x, probability = TRUE, xlab = "Waiting times (in min.)", border = "gray", xlim = range(rx), ylim = c(0, 0.06), main = "")
lines(rx, f, lwd = 2)
lines(rx, dnorm(rx, mean = mean(x), sd = sd(x)), lty = 2, lwd = 2)
legend(50, 0.06, legend = c("Fitted two-component mixture density", "Fitted single normal density"), lty = 1:2, bty = "n")

detach("package:flexmix", unload=TRUE)
bootpara <- boot(faithful$waiting, fit, R = 1000)
boot.ci(bootpara, type = "bca", index = 1)
boot.ci(bootpara, type = "bca", index = 2)
boot.ci(bootpara, type = "bca", index = 3)
layout(matrix(1:2, ncol = 2))
bootplot(bootpara, 2, main = expression(mu[1]))
bootplot(bootpara, 3, main = expression(mu[2]))
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
