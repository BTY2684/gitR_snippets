memory.limit(4000)

pkgImport("boot")
pkgImport("ggplot2")
pkgImport("reshape2")

boot.max <- function(data, indices)
{
  d <- data[indices, ]
  return(sapply(d, function(x) max(x) ) )
}

boot.mean <- function(data, indices)
{
  d <- data[indices, ]
  return(sapply(d, function(x) boot.mean(x) ) )
}

boot.median <- function(data, indices)
{
  d <- data[indices, ]
  return(sapply(d, function(x) boot.median(x) ) )
}

boot.sum <- function(data, indices)
{
  d <- data[indices, ]
  return(sapply(d, function(x) sum(x) ) )
}

# df <- read.csv("Z:/tempData/FR_LGD_Boot_data.csv")

df.ret <- read.csv("Z:/tempData/FR_LGD_Boot_ret.csv")

raw.val <- read.csv("Z:/tempData/HBFR_LGD_Val.csv")

df.val <- raw.val[raw.val$Obs!=0,]

summary(df.ret)

# head(df)
head(df.ret)
head(raw.val)

ggplot(df.ret[,c(1,4)], aes(y=Est_Obs_Ratio, x=Replicate)) + geom_point() + geom_hline(aes(yintercept=mean(df.val$Est/df.val$Obs)), color="red", size=1, linetype="dashed", alpha = 0.6)

ggplot(df.ret[,c(1,4)], aes(Est_Obs_Ratio)) + geom_histogram(alpha = 0.6, aes(y=..density..), binwidth=0.1) + geom_density(alpha=.2, size=1) + geom_vline(aes(xintercept=mean(df.val$Est/df.val$Obs)), color="red", size=1, linetype="dashed", alpha = 0.6) + geom_vline(aes(xintercept=quantile(Est_Obs_Ratio, .025)), color="black", size=1, linetype="dashed", alpha = 0.6) + geom_vline(aes(xintercept=quantile(Est_Obs_Ratio, .975)), color="black", size=1, linetype="dashed", alpha = 0.6)

# + geom_vline(aes(xintercept=mean(df.ret$Est_Obs_Ratio)), color="blue", size=1, linetype="dashed", alpha = 0.6)

v.plot <- melt(df.ret[,c(-1, -4)])
Obs.Loss <- sum(df.val$Obs*df.val$Ead)
mean.Est.Loss <- mean(df.ret$Total_Est_Loss)
mean.Obs.Loss <- mean(df.ret$Total_Obs_Loss)
head(v.plot)

p<- ggplot(v.plot, aes(value/1000000, fill = factor(variable))) + geom_density(alpha=.2, aes(linetype=factor(variable)), size=1)

# + geom_histogram(alpha = 0.6, aes(y=..density..), binwidth=5) 

p <- p + geom_vline(aes(xintercept=Obs.Loss/1000000), color="red", size=1, linetype="dashed", alpha = 0.6)

p <- p + geom_vline(aes(xintercept=mean.Est.Loss/1000000), color="blue", size=1, linetype="solid", alpha = 0.6)

p <- p + geom_vline(aes(xintercept=mean.Obs.Loss/1000000), color="green", size=1, linetype="solid", alpha = 0.6)

p <- p + geom_vline(aes(xintercept=quantile(df.ret$Total_Est_Loss/1000000, .025)), color="black", size=1, linetype="dashed", alpha = 0.6)

p <- p + geom_vline(aes(xintercept=quantile(df.ret$Total_Est_Loss/1000000, .05)), color="black", size=1, linetype="dashed", alpha = 0.6)

p <- p + geom_vline(aes(xintercept=quantile(df.ret$Total_Est_Loss/1000000, .075)), color="black", size=1, linetype="dashed", alpha = 0.6)

p
