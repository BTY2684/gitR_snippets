pkgImport("lme4")

data.df <- read.csv("c:/test/testLoss.csv")
head(data.df)

# null model

(fit.null <- glm(L_rate ~ 1 + R_ind + R_sec + B_ad, data = data.df))
summary(fit.null)

# independent random effects

(fit <- lmer(L_rate ~ 1 + R_ind + (1|T_ind) + R_sec + (1|T_sec) + B_ad + (1|T_prod), data.df, REML=FALSE))
summary(fit)

fixef(fit)
ranef(fit)
coef(fit)

est <- fitted(fit)
summary(est)

residuals <- resid(fit)
summary(residuals)
hist(residuals)

mcmc.5000 <- mcmcsamp(fit, saveb = TRUE, n = 5000)
str(mcmc.5000)

summary(mcmc.5000@deviance)

# correlated random effects

(fit2 <- lmer(L_rate ~ 1 + R_ind + R_sec + B_ad + (1|T_ind/T_sec/T_prod), data.df, REML=F))
summary(fit2)

fixef(fit2)
ranef(fit2)
coef(fit2)

est2 <- fitted(fit2)
summary(est2)

residuals2 <- resid(fit2)
summary(residuals2)
hist(residuals2)

mcmc2.5000 <- mcmcsamp(fit2, saveb = TRUE, n = 5000)

# customized model

(fit3 <- lmer(L_rate ~ 1 + R_ind + R_sec + (1|T_ind/T_sec/T_prod), data.df, REML=F))
summary(fit3)

fixef(fit3)
ranef(fit3)
coef(fit3)

est3 <- fitted(fit3)
summary(est3)

residuals3 <- resid(fit3)
summary(residuals3)
hist(residuals3)

mcmc3.5000 <- mcmcsamp(fit3, saveb = TRUE, n = 5000)
