library(aod)
library(ggplot2)
library(effects)

summary(diamonds)

diam_data <- diamonds
diam_data$priceRank <- factor(round(log(diam_data$price), digits = 0) - 5)
head(diam_data)
unique(diam_data$priceRank)

tmp_data <- melt(diam_data[, c("priceRank", "price", "carat", "cut", "color", "clarity")], id.vars=c("priceRank", "cut", "color", "clarity"))

ggplot(tmp_data, aes(x = carat, y = price, colour = priceRank)) +
  geom_point(alpha = .1) + facet_grid(variable ~ value)

ggplot(diam_data, aes(x = carat, y = price, colour = priceRank)) +
  geom_point(alpha = .1) + facet_grid(cut ~ .)

model_melogit <- glmer(priceRank ~ (1|carat) + cut + color + clarity, data = diam_data, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
print(model_melogit, corr = FALSE)
(se <- sqrt(diag(vcov(model_melogit))))
(tab <- cbind(Est = fixef(model_melogit), LL = fixef(model_melogit) - 1.96 * se, UL = fixef(model_melogit) + 1.96 * se))

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
tmp <- sampler(diam_data, "carat", reps = 10)

bigdata <- cbind(tmp, diam_data[tmp$RowID, ])

f <- fixef(model_melogit)
r <- getME(model_melogit, "theta")

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
tmpdat <- diam_data[, c("priceRank", "price", "carat", "cut", "color", "clarity")]

summary(diam_data$price)

jvalues <- with(diam_data, seq(from = min(price), to = max(price), length.out = 100))

# calculate predicted probabilities and store in a list
pp <- lapply(jvalues, function(j) {
  tmpdat$price <- j
  predict(model_melogit, newdata = tmpdat, type = "response")
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
colnames(plotdat) <- c("PredictedProbability", "Lower", "Upper", "price")
head(plotdat)

# plot average marginal predicted probabilities
ggplot(plotdat, aes(x = price, y = PredictedProbability)) + geom_line() +
  ylim(c(0, 1))

ggplot(plotdat, aes(x = price, y = PredictedProbability)) + geom_linerange(aes(ymin = Lower, ymax = Upper)) + geom_line(size = 2) + ylim(c(0, 1))

# calculate predicted probabilities and store in a list
biprobs <- lapply(levels(diam_data$cut), function(stage) {
  tmpdat$cut[] <- stage
  lapply(jvalues, function(j) {
    tmpdat$price <- j
    predict(model_melogit, newdata = tmpdat, type = "response")
  })
})

# get means and quartiles for all jvalues for each level of CancerStage
plotdat2 <- lapply(biprobs, function(X) {
  temp <- t(sapply(X, function(x) {
    c(M=mean(x), quantile(x, c(.25, .75)))
  }))
  temp <- as.data.frame(cbind(temp, jvalues))
  colnames(temp) <- c("PredictedProbability", "Lower", "Upper", "price")
  return(temp)
})

# collapse to one data frame
plotdat2 <- do.call(rbind, plotdat2)

# add cancer stage
plotdat2$cut <- factor(rep(levels(diam_data$cut), each = length(jvalues)))

# show first few rows
head(plotdat2)

# graph it
ggplot(plotdat2, aes(x = price, y = PredictedProbability)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = cut), alpha = .15) +
  geom_line(aes(colour = cut), size = 2) +
  ylim(c(0, 1)) + facet_wrap(~  cut)

ggplot(data.frame(Probs = biprobs[[4]][[100]]), aes(Probs)) + geom_histogram() +
  scale_x_sqrt(breaks = c(0.01, 0.1, 0.25, 0.5, 0.75))

################################################## logit
model_logit <- glm(priceRank ~ 0 + carat + cut + color + clarity, data = diam_data, family = binomial(logit))
summary(model_logit)
confint(model_logit)
confint.default(model_logit)
fitted(model_logit)

ggplot(diam_data, aes(x = carat, y = price)) + facet_grid(cut ~ .) + geom_point(aes(colour = clarity, size = 1)) + stat_smooth()

ggplot(diam_data, aes(x = carat, y = price, color = priceRank)) + geom_point() + stat_smooth(method = lm) + facet_grid(priceRank ~ .)

ggplot(diam_data, aes(x = carat, y = price, color = clarity)) + stat_smooth() + facet_grid(clarity ~ .) + facet_grid(clarity ~ cut) + geom_point()

model_linear <- glm(price ~ 0 + carat + cut + color + clarity, data = diam_data, family = gaussian)
summary(model_linear)
confint(model_linear)
confint.default(model_linear)
fitted(model_linear)

ggplot(diam_data, aes(x = price, y = fitted(model_linear), color = clarity)) + geom_line() + facet_grid(clarity ~ .) + stat_smooth()

model_exp <- nls(price ~ exp(a + b * carat ), data = na.omit(diam_data), start = list(a = 5, b = 5))
summary(model_exp)
confint(model_exp)
confint.default(model_exp)
fitted(model_exp)

ggplot(diam_data, aes(x = price, y = fitted(model_exp), color = clarity)) + geom_point() + facet_grid(. ~ clarity) + stat_smooth()







####################################
wald.test(b = coef(model_logit), Sigma = vcov(model_logit), Terms = c(2:6, 7:12, 13:19) )

l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(model_logit), Sigma = vcov(model_logit), L = l)

exp(coef(model_logit))

exp(cbind(OR = coef(model_logit), confint(model_logit)))

?exp

predicted_priceRank <- data.frame(response = predict(model_logit, newdata = diam_data, type = "response"), link = round(predict(model_logit, newdata = diam_data, type = "link"), digits = 0))

unique(predict(model_logit, newdata = diam_data, type = "response"))
unique(round(predict(model_logit, newdata = diam_data, type = "link"), digits = 0))

ggplot(predicted_priceRank, aes(x = log(link), y = response)) + geom_line()

################################


mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)
summary(mydata)

sapply(mydata, sd)
xtabs(~admit + rank, data = mydata)

mydata$rank <- factor(mydata$rank)
# mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
# summary(mylogit)
# confint(mylogit)
# confint.default(mylogit)


mylogit_rank <- glm(rank ~ 0 + gre + gpa, data = mydata, family = binomial(logit))
summary(mylogit_rank)
confint(mylogit_rank)
confint.default(mylogit_rank)
fitted(mylogit_rank)

mydata[order(mydata$rank),]

ggplot(mydata, aes(x = gpa, y = rank)) + geom_point(aes(colour = factor(ceiling(gpa)), size = 2)) + facet_grid(rank ~ . )

newdata <- cbind(mydata, predict(mylogit_rank, newdata = mydata, type = "link", se = TRUE))

ggplot(newdata, aes(x = gpa, y = rank)) + geom_point(aes(colour = factor(ceiling(fit))), size = 2) + facet_grid(rank ~ . )

result <- data.frame('fitted' = newdata$fit, 'fitted_round' = round(newdata$fit, digits = 0), 'rank' = newdata$rank)

wald.test(b = coef(mylogit_rank), Sigma = vcov(mylogit_rank), Terms = 4:6)

l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit_rank), Sigma = vcov(mylogit_rank), L = l)

exp(coef(mylogit_rank))

exp(cbind(OR = coef(mylogit_rank), confint(mylogit_rank)))

newdata_1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa)))
newdata_1$rankP <- predict(mylogit_rank, newdata = newdata_1, type = "response")

newdata_2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 600), 1), gpa = rep(seq(from = 2, to = 4, length.out = 600), 1), rank = factor(rep(1:4, each = 150))))

newdata_3 <- cbind(newdata_2, predict(mylogit_rank, newdata = newdata_2, type = "link", se = TRUE))
newdata_3$rankP <- round(predict(mylogit_rank, newdata = newdata_2, type = "link"), digits = 0)
newdata_3 <- within(newdata_3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata_3)
newdata_3$PredictedProb

ggplot(newdata_3, aes(x = gre, y = gpa)) + geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rankP), size = 2) + facet_grid(rank ~ .)

with(mylogit_rank, null.deviance - deviance)
with(mylogit_rank, df.null - df.residual)
with(mylogit_rank, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(mylogit_rank)
