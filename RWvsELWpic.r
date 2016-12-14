rw<-function(pd=0.02, lgd=0.4, m=1)
{
  R = 0.12 * (1- exp(-50*(pd)))/(1-exp(-50))+ 0.24 * (1-(1-exp(-50*(pd)))/(1-exp(-50)))
  b = (0.11852 - 0.05478 * log((pd)))^2
  K = (lgd * pnorm(qnorm(pd)/sqrt(1-R) + sqrt(R/(1-R)) * qnorm(0.999))-pd*lgd) / (1-1.5*b) * (1 +(m - 2.5) * b)
  RW = K * 1.06
  return(RW)
} 

rw.pd1<-function(pd1=0.02, pd2=0.02, lgd=0.4, m=1)
{
  R = 0.12 * (1- exp(-50*(pd1)))/(1-exp(-50))+ 0.24 * (1-(1-exp(-50*(pd1)))/(1-exp(-50)))
  b = (0.11852 - 0.05478 * log((pd1)))^2
  K = (lgd * pnorm(qnorm(pd1)/sqrt(1-R) + sqrt(R/(1-R)) * qnorm(0.999))-pd2*lgd) / (1-1.5*b) * (1 +(m - 2.5) * b)
  RW = K * 1.06
  return(RW)
} 

rw.pd2<-function(pd1=0.02, pd2=0.02, lgd=0.4, m=1)
{
  R = 0.12 * (1- exp(-50*(pd2)))/(1-exp(-50))+ 0.24 * (1-(1-exp(-50*(pd2)))/(1-exp(-50)))
  b = (0.11852 - 0.05478 * log((pd2)))^2
  K = (lgd * pnorm(qnorm(pd2)/sqrt(1-R) + sqrt(R/(1-R)) * qnorm(0.999))-pd1*lgd) / (1-1.5*b) * (1 +(m - 2.5) * b)
  RW = K * 1.06
  return(RW)
} 

crr <- c('1.1', '1.2', '2.1', '2.2', '3.1', '3.2', '3.3', '4.1', '4.2', '4.3', '5.1', '5.2', '5.3', '6.1', '6.2', '7.1', '7.2', '8.1', '8.2', '8.3')
pd <- c(0.0002, 0.0004, 0.0007, 0.0013, 0.0022, 0.0037, 0.0063, 0.0087, 0.012, 0.0165, 0.0225, 0.0305, 0.042, 0.0575, 0.0785, 0.1, 0.13, 0.19, 0.36, 0.75)
lgd <- seq(0.05, 1, 0.05)

df <- expand.grid(pd1 = pd, pd2 = pd, lgd = lgd, KEEP.OUT.ATTRS = FALSE)
# df$ELW.1 <- df$pd1*df$lgd
# df$ELW.2 <- df$pd2*df$lgd
# df$RW.high <-rw(df$pd1, df$lgd)
# df$RW.low <-rw(df$pd2, df$lgd)
df$RW.1 <-rw.pd1(df$pd1, df$pd2, df$lgd)
df$RW.2 <-rw.pd2(df$pd1, df$pd2, df$lgd)

df.melt.0 <- melt(df,id=c("pd1", "pd2","lgd"))

df.melt <- df.melt.0[df.melt.0$pd1>df.melt.0$pd2,]

head(df.melt)

var_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="pd1") { 
    value[value==0.0002] <- "CRR 1.1"
    value[value==0.0004] <- "CRR 1.2"
    value[value==0.0007] <- "CRR 2.1"
    value[value=="0.0013"] <- "CRR 2.2"
    value[value=="0.0022"] <- "CRR 3.1"
    value[value=="0.0037"] <- "CRR 3.2"
    value[value=="0.0063"] <- "CRR 3.3"
    value[value=="0.0087"] <- "CRR 4.1"
    value[value=="0.012"] <- "CRR 4.2"
    value[value=="0.0165"] <- "CRR 4.3"
    value[value=="0.0225"] <- "CRR 5.1"
    value[value=="0.0305"] <- "CRR 5.2"
    value[value=="0.042"] <- "CRR 5.3"
    value[value=="0.0575"] <- "CRR 6.1"
    value[value=="0.0785"] <- "CRR 6.2"
    value[value=="0.1"] <- "CRR 7.1"
    value[value=="0.13"] <- "CRR 7.2"
    value[value=="0.19"] <- "CRR 8.1"
    value[value=="0.36"] <- "CRR 8.2"
    value[value=="0.75"] <- "CRR 8.3"
  }
  
  if (var=="lgd") { 
    value[value=="0.05"] <- "LGD = 0.05 (5%)"
    value[value=="0.1"] <- "LGD = 0.1 (10%"
    value[value=="0.15"] <- "LGD = 0.15 (15%)"
    value[value=="0.2"] <- "LGD = 0.2 (20%)"
    value[value=="0.25"] <- "LGD = 0.25 (25%)"
    value[value=="0.3"] <- "LGD = 0.3 (30%)"
    value[value=="0.35"] <- "LGD = 0.35 (35%)"
    value[value=="0.4"] <- "LGD = 0.4 (40%)"
    value[value=="0.45"] <- "LGD = 0.45 (45%)"
    value[value=="0.5"] <- "LGD = 0.5 (50%)"
    value[value=="0.55"] <- "LGD = 0.55 (55%)"
    value[value=="0.6"] <- "LGD = 0.6 (60%)"
    value[value=="0.65"] <- "LGD = 0.65 (65%)"
    value[value=="0.7"] <- "LGD = 0.7 (70%)"
    value[value=="0.75"] <- "LGD = 0.75 (75%)"
    value[value=="0.8"] <- "LGD = 0.8 (80%)"
    value[value=="0.85"] <- "LGD = 0.85 (85%)"
    value[value=="0.9"] <- "LGD = 0.9 (90%)"
    value[value=="0.95"] <- "LGD = 0.95 (95%)"
    value[value=="1"] <- "LGD = 1 (100%)"
  }
  if (var=="variable") { 
    value[value=="ELW"] <- "PD * LGD"
    value[value=="RW"] <- "Risk Weight"
    value[value=="RW.1"] <- "Risk Weight PD1"
    value[value=="RW.2"] <- "Risk Weight PD2"
  }
  return(value)
}

df.melt$Legend <- var_labeller('variable', df.melt$variable)
df.melt$pd.Lab <- var_labeller('pd1', df.melt$pd1)
df.melt$lgd.Lab <- var_labeller('lgd', df.melt$lgd)

head(df.melt[df.melt$variable=='RW.low' & df.melt$pd.Lab=='CRR 8.2',])

d <- ggplot(df.melt, aes(x = lgd, y = value, colour = Legend)) + geom_line() + theme(aspect.ratio = 1)
d + facet_wrap(~ pd.Lab)

d1 <- ggplot(df.melt[df.melt$pd==0.19,], aes(x = lgd, y = value, colour = Legend)) + geom_line() + theme(aspect.ratio = 1)
d1 <- d1 + labs(x = "LGD", y = "Percentage Value")
d1 <- d1 + ggtitle("PD * LGD vs. Risk Weight\nfor CRR 8.1 where PD = 19%") + theme(plot.title = element_text(lineheight=.8, face="bold"))

d1 <- d1 + geom_vline(aes(xintercept=0.45), color="brown", size=1, linetype="dashed", alpha = 0.6)
d1 <- d1 + geom_point(aes(x=0.45, y=0), color="blue", size=3, alpha = 0.6) + geom_text(mapping=aes(x=0.45, y=0, label="A"), color="blue", size=4, angle=0, vjust=0, hjust=0)
d1 <- d1 + geom_point(aes(x=0.45, y=df.melt$value[df.melt$pd==0.19&df.melt$lgd==0.45&df.melt$variable=='ELW']), color="blue", size=3, alpha = 0.6) + geom_text(mapping=aes(x=0.45, y=df.melt$value[df.melt$pd==0.19&df.melt$lgd==0.45&df.melt$variable=='ELW'], label="A1"), color="blue", size=4, angle=0, vjust=0, hjust=0)
d1 <- d1 + geom_point(aes(x=0.45, y=df.melt$value[df.melt$pd==0.19&df.melt$lgd==0.45&df.melt$variable=='RW']), color="blue", size=3, alpha = 0.6) + geom_text(mapping=aes(x=0.45, y=df.melt$value[df.melt$pd==0.19&df.melt$lgd==0.45&df.melt$variable=='RW'], label="A2"), color="blue", size=4, angle=0, vjust=0, hjust=0)

d1 <- d1 + geom_vline(aes(xintercept=0.75), color="black", size=1, linetype="dashed", alpha = 0.6)
d1 <- d1 + geom_point(aes(x=0.75, y=0), color="blue", size=3, alpha = 0.6) + geom_text(mapping=aes(x=0.75, y=0, label="B"), color="blue", size=4, angle=0, vjust=0, hjust=0)
d1 <- d1 + geom_point(aes(x=0.75, y=df.melt$value[df.melt$pd==0.19&df.melt$lgd=="0.75"&df.melt$variable=='ELW']), color="blue", size=3, alpha = 0.6) + geom_text(mapping=aes(x=0.75, y=df.melt$value[df.melt$pd==0.19&df.melt$lgd=="0.75"&df.melt$variable=='ELW'], label="B1"), color="blue", size=4, angle=0, vjust=0, hjust=0)
d1 <- d1 + geom_point(aes(x=0.75, y=df.melt$value[df.melt$pd==0.19&df.melt$lgd=="0.75"&df.melt$variable=='RW']), color="blue", size=3, alpha = 0.6) + geom_text(mapping=aes(x=0.75, y=df.melt$value[df.melt$pd==0.19&df.melt$lgd=="0.75"&df.melt$variable=='RW'], label="B2"), color="blue", size=4, angle=0, vjust=0, hjust=0)


d1 <- d1 + geom_hline(aes(yintercept=df.melt$value[df.melt$pd==0.19&df.melt$lgd==0.45&df.melt$variable=='ELW']), color="brown", size=1, linetype="dashed", alpha = 0.6)
d1 <- d1 + geom_point(aes(x=0.75, y=df.melt$value[df.melt$pd==0.19&df.melt$lgd==0.45&df.melt$variable=='ELW']), color="blue", size=3, alpha = 0.6) + geom_text(mapping=aes(x=0.75, y=df.melt$value[df.melt$pd==0.19&df.melt$lgd==0.45&df.melt$variable=='ELW'], label="A1*"), color="blue", size=4, angle=0, vjust=0, hjust=0)

d1 <- d1 + geom_hline(aes(yintercept=df.melt$value[df.melt$pd==0.19&df.melt$lgd==0.45&df.melt$variable=='RW']), color="brown", size=1, linetype="dashed", alpha = 0.6)
d1 <- d1 + geom_point(aes(x=0.75, y=df.melt$value[df.melt$pd==0.19&df.melt$lgd==0.45&df.melt$variable=='RW']), color="blue", size=3, alpha = 0.6) + geom_text(mapping=aes(x=0.75, y=df.melt$value[df.melt$pd==0.19&df.melt$lgd==0.45&df.melt$variable=='RW'], label="A2*"), color="blue", size=4, angle=0, vjust=0, hjust=0)

d1 <- d1 + geom_segment(aes(x = 0.75, y = df.melt$value[df.melt$pd==0.19&df.melt$lgd==0.45&df.melt$variable=='ELW'], xend = 0.75, yend = df.melt$value[df.melt$pd==0.19&df.melt$lgd=="0.75"&df.melt$variable=='ELW']), color="green", size=1)

d1 <- d1 + geom_segment(aes(x = 0.75, y = df.melt$value[df.melt$pd==0.19&df.melt$lgd==0.45&df.melt$variable=='RW'], xend = 0.75, yend = df.melt$value[df.melt$pd==0.19&df.melt$lgd=="0.75"&df.melt$variable=='RW']), color="green", size=1)

d1
  

p <- ggplot(df.melt, aes(x = pd1, y = value, colour = Legend)) + geom_line() + theme(aspect.ratio = 1) 
p + facet_wrap(~ lgd.Lab)


p1 <- ggplot(df.melt[df.melt$lgd==0.45,], aes(x = pd, y = value, colour = Legend)) + geom_line() + theme(aspect.ratio = 1)
p1 <- p1 + labs(x = "PD", y = "Percentage Value")
p1 <- p1 + ggtitle("PD * LGD vs. Risk Weight\nfor LGD = 45%") + theme(plot.title = element_text(lineheight=.8, face="bold"))

p1 <- p1 + geom_vline(aes(xintercept=0.36), color="black", size=1, alpha = 0.6)

p1 <- p1 + geom_point(aes(x=df.melt$pd[df.melt$lgd==0.45&df.melt$variable=='RW'], y=df.melt$value[df.melt$lgd==0.45&df.melt$variable=='RW']), color="red", size=3, alpha = 0.6)

p1

################################################ 
# On continuous PD scale
################################################
pd.c <- seq(0.0001, 0.9999, 0.0001)
lgd.c <- seq(0.01, 1.2, 0.01)

df.c <- expand.grid(pd = pd.c, lgd = lgd.c, KEEP.OUT.ATTRS = FALSE)
df.c$ELW <- df.c$pd*df.c$lgd
df.c$RW <-rw(df.c$pd, df.c$lgd)

head(df.c)

df.c.melt <- melt(df.c,id=c("pd","lgd"))

head(df.c.melt)

df.c.melt$Legend <- var_labeller('variable', df.c.melt$variable)
df.c.melt$pd.Lab <- var_labeller('pd', df.c.melt$pd)
df.c.melt$lgd.Lab <- var_labeller('lgd', df.c.melt$lgd)

head(df.c.melt)

df.c.melt[df.c.melt$lgd=='0.6'&df.c.melt$value==max(df.c$RW[df.c$lgd=='0.6']),]

df.c.melt[df.c.melt$lgd=='0.6'&df.c.melt$pd==0.3098,]

p2 <- ggplot(df.c.melt[df.c.melt$lgd==0.45,], aes(x = pd, y = value, colour = Legend)) + geom_line() + theme(aspect.ratio = 1)
p2 <- p2 + labs(x = "PD", y = "Percentage Value")
p2 <- p2 + ggtitle("Continuous Scale:\nPD * LGD vs. Risk Weight\nfor LGD = 45%") + theme(plot.title = element_text(lineheight=.8, face="bold"))

p2 <- p2 + geom_vline(aes(xintercept=0.3098), color="black", size=1,linetype="dashed", alpha = 0.6) + geom_text(mapping=aes(x=0.3098, y=0, label="PD = 31%"), color="blue", size=4, angle=90, vjust=1, hjust=0)

p2
