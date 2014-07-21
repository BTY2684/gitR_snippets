library(aod)
library(ggplot2)
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


mylogit_rank <- glm(rank ~ gre + gpa, data = mydata, family = binomial(logit))
summary(mylogit_rank)
confint(mylogit_rank)
confint.default(mylogit_rank)
fitted(mylogit_rank)

newdata <- cbind(mydata, predict(mylogit_rank, newdata = mydata, type = "link", se = TRUE))

ggplot(newdata, aes(x = gre, y = fit)) + geom_point(aes(colour = factor(round(fit, digits = 0))), size = 2) + facet_grid(rank ~ . )

result <- data.frame('fitted' = newdata$fit, 'fitted_round' = round(newdata$fit, digits = 0), 'rank' = newdata$rank)

wald.test(b = coef(mylogit_rank), Sigma = vcov(mylogit_rank), Terms = 4:6)

l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit_rank), Sigma = vcov(mylogit_rank), L = l)

exp(coef(mylogit_rank))

exp(cbind(OR = coef(mylogit_rank), confint(mylogit_rank)))

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata1$rankP <- predict(mylogit_rank, newdata = newdata1, type = "response")

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

newdata3 <- cbind(newdata2, predict(mylogit_rank, newdata = newdata2, type = "link", se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata3)

ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank), size = 1)

with(mylogit_rank, null.deviance - deviance)
with(mylogit_rank, df.null - df.residual)
with(mylogit_rank, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(mylogit_rank)
