pkgImport("ggplot2")
pkgImport("GGally")
pkgImport("reshape2")
pkgImport("lme4")
pkgImport("compiler")
pkgImport("parallel")
pkgImport("boot")

hdp <- read.csv("http://www.ats.ucla.edu/stat/data/hdp.csv")
hdp <- within(hdp, {
  Married <- factor(Married, levels = 0:1, labels = c("no", "yes"))
  DID <- factor(DID)
  HID <- factor(HID)
})

ggpairs(hdp[, c("IL6", "CRP", "LengthofStay", "Experience")])

ggplot(hdp, aes(x = CancerStage, y = LengthofStay)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=10)

tmp <- melt(hdp[, c("CancerStage", "IL6", "CRP")], id.vars="CancerStage")
ggplot(tmp, aes(x = CancerStage, y = value)) +
  geom_jitter(alpha = .1) +
  geom_violin(alpha = .75) +
  facet_grid(variable ~ .) +
  scale_y_sqrt()

tmp <- melt(hdp[, c("remission", "IL6", "CRP", "LengthofStay", "Experience")], id.vars="remission")
ggplot(tmp, aes(factor(remission), y = value, fill=factor(remission))) +
  geom_boxplot() + facet_wrap(~variable, scales="free_y")

# Below is a list of analysis methods you may have considered.
#
# Mixed effects logistic regression, the focus of this page.
#
# Mixed effects probit regression is very similar to mixed effects logistic regression, but it uses the normal CDF instead of the logistic CDF. Both model binary outcomes and can include fixed and random effects.
#
# Fixed effects logistic regression is limited in this case because it may ignore necessary random effects and/or non independence in the data.
#
# Fixed effects probit regression is limited in this case because it may ignore necessary random effects and/or non independence in the data.
#
# Logistic regression with clustered standard errors. These can adjust for non independence but does not allow for random effects.
#
# Probit regression with clustered standard errors. These can adjust for non independence but does not allow for random effects.

# estimate the model and store results in m
m <- glmer(remission ~ IL6 + CRP + CancerStage + LengthofStay + Experience +
             (1 | DID), data = hdp, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

# print the mod results without correlations among fixed effects
print(m, corr = FALSE)

se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
                se))

exp(tab)

# Multilevel bootstrapping
sampler <- function(dat, clustervar, replace = TRUE, reps = 1) {
  cid <- unique(dat[, clustervar[1]])
  ncid <- length(cid)
  recid <- sample(cid, size = ncid * reps, replace = TRUE)
  if (replace) {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = sample(which(dat[, clustervar] == recid[i]),
                                      size = length(which(dat[, clustervar] == recid[i])), replace = TRUE))
    })
  } else {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = which(dat[, clustervar] == recid[i]))
    })
  }
  dat <- as.data.frame(do.call(rbind, rid))
  dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:reps), include.lowest = TRUE,
                              labels = FALSE))
  dat$NewID <- factor(dat$NewID)
  return(dat)
}

set.seed(20)
tmp <- sampler(hdp, "DID", reps = 10)


##########  begin test  ##########
cid <- unique(hdp[, "DID"])
ncid <- length(cid)
recid <- sample(cid, size = ncid * 10, replace = TRUE)

  rid <- lapply(seq_along(recid), function(i) {
    cbind(NewID = i, RowID = sample(which(hdp[, "DID"] == recid[i]),
                                    size = length(which(hdp[, "DID"] == recid[i])), replace = TRUE))
  })

dat <- as.data.frame(do.call(rbind, rid))
dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:10), include.lowest = TRUE, labels = FALSE))
dat$NewID <- factor(dat$NewID)
##########  end test  ##########


bigdata <- cbind(tmp, hdp[tmp$RowID, ])

f <- fixef(m)
r <- getME(m, "theta")

cl <- makeCluster(4)
clusterExport(cl, c("bigdata", "f", "r"))
clusterEvalQ(cl, require(lme4))

myboot <- function(i) {
  object <- try(glmer(remission ~ IL6 + CRP + CancerStage + LengthofStay +
                        Experience + (1 | NewID), data = bigdata, subset = Replicate == i, family = binomial,
                      nAGQ = 1, start = list(fixef = f, theta = r)), silent = TRUE)
  if (class(object) == "try-error")
    return(object)
  c(fixef(object), getME(object, "theta"))
}

start <- proc.time()
res <- parLapplyLB(cl, X = levels(bigdata$Replicate), fun = myboot)
end <- proc.time()

# shut down the cluster
stopCluster(cl)

# calculate proportion of models that successfully converged
success <- sapply(res, is.numeric)
mean(success)

# combine successful results
bigres <- do.call(cbind, res[success])

# calculate 2.5th and 97.5th percentiles for 95% CI
(ci <- t(apply(bigres, 1, quantile, probs = c(0.025, 0.975))))

# All results
finaltable <- cbind(Est = c(f, r), SE = c(se, NA), BootMean = rowMeans(bigres),
                    ci)
# round and print
round(finaltable, 3)

# Predicted probabilities and graphing

# temporary data
tmpdat <- hdp[, c("IL6", "CRP", "CancerStage", "LengthofStay", "Experience",
                  "DID")]

summary(hdp$LengthofStay)

jvalues <- with(hdp, seq(from = min(LengthofStay), to = max(LengthofStay), length.out = 100))

# calculate predicted probabilities and store in a list
pp <- lapply(jvalues, function(j) {
  tmpdat$LengthofStay <- j
  predict(m, newdata = tmpdat, type = "response")
})

# average marginal predicted probability across a few different Lengths of
# Stay
sapply(pp[c(1, 20, 40, 60, 80, 100)], mean)

# get the means with lower and upper quartiles
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))

# add in LengthofStay values and convert to data frame
plotdat <- as.data.frame(cbind(plotdat, jvalues))

# better names and show the first few rows
colnames(plotdat) <- c("PredictedProbability", "Lower", "Upper", "LengthofStay")
head(plotdat)

# plot average marginal predicted probabilities
ggplot(plotdat, aes(x = LengthofStay, y = PredictedProbability)) + geom_line() +
  ylim(c(0, 1))

ggplot(plotdat, aes(x = LengthofStay, y = PredictedProbability)) + geom_linerange(aes(ymin = Lower, ymax = Upper)) + geom_line(size = 2) + ylim(c(0, 1))

# calculate predicted probabilities and store in a list
biprobs <- lapply(levels(hdp$CancerStage), function(stage) {
  tmpdat$CancerStage[] <- stage
  lapply(jvalues, function(j) {
    tmpdat$LengthofStay <- j
    predict(m, newdata = tmpdat, type = "response")
  })
})

# get means and quartiles for all jvalues for each level of CancerStage
plotdat2 <- lapply(biprobs, function(X) {
  temp <- t(sapply(X, function(x) {
    c(M=mean(x), quantile(x, c(.25, .75)))
  }))
  temp <- as.data.frame(cbind(temp, jvalues))
  colnames(temp) <- c("PredictedProbability", "Lower", "Upper", "LengthofStay")
  return(temp)
})

# collapse to one data frame
plotdat2 <- do.call(rbind, plotdat2)

# add cancer stage
plotdat2$CancerStage <- factor(rep(levels(hdp$CancerStage), each = length(jvalues)))

# show first few rows
head(plotdat2)

# graph it
ggplot(plotdat2, aes(x = LengthofStay, y = PredictedProbability)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = CancerStage), alpha = .15) +
  geom_line(aes(colour = CancerStage), size = 2) +
  ylim(c(0, 1)) + facet_wrap(~  CancerStage)

ggplot(data.frame(Probs = biprobs[[4]][[100]]), aes(Probs)) + geom_histogram() +
  scale_x_sqrt(breaks = c(0.01, 0.1, 0.25, 0.5, 0.75))

# Three level mixed effects logistic regression

# estimate the model and store results in m
m3a <- glmer(remission ~ Age + LengthofStay + FamilyHx + IL6 + CRP +
               CancerStage + Experience + (1 | DID) + (1 | HID),
             data = hdp, family = binomial, nAGQ=1)

# print the mod results without correlations among fixed effects
print(m3a, corr=FALSE)

dotplot(ranef(m3a, which = "DID", postVar = TRUE), scales = list(y = list(alternating = 0)))

dotplot(ranef(m3a, which = "HID", postVar = TRUE))

# estimate the model and store results in m
m3b <- glmer(remission ~ Age + LengthofStay + FamilyHx + IL6 + CRP + CancerStage +
               Experience + (1 + LengthofStay | DID) + (1 | HID), data = hdp, family = binomial,
             nAGQ = 1)

# print the mod results without correlations among fixed effects
print(m3b, corr = FALSE)

dotplot(ranef(m3b, which = "DID", postVar = TRUE), scales = list(y = list(alternating = 0)))

dotplot(ranef(m3b, which = "HID", postVar = TRUE), scales = list(y = list(alternating = 0)))
