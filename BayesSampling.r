(WD <- getwd())

###
# Included libraries
###
pkgImport("LearnBayes")
pkgImport("parallel")
pkgImport("bnlearn")
pkgImport("deal")


###
# RStan Example
###
# library(rstan)
# schools_dat <- list(J = 8, 
#                     y = c(28,  8, -3,  7, -1,  1, 18, 12),
#                     sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
# 
# fit <- stan(file = '8schools.rstan', data = schools_dat, 
#             iter = 1000, chains = 4)
# 
# # 
# # This is a demo of using model_code argument since 
# # we can use this file directly or put the string in R 
# # directly. 
# #  
# schools_code <- paste(readLines('8schools.rstan'), collapse = '\n')
# fit1 <- stan(model_code = schools_code, data = schools_dat, 
#              iter = 1000, chains = 4)
# 
# fit2 <- stan(fit = fit1, data = schools_dat, iter = 10000, chains = 4)
# 
# print(fit2)
# plot(fit2)
# 
# la <- extract(fit2, permuted = TRUE) # return a list of arrays 
# mu <- la$mu 
# 
# ### return an array of three dimensions: iterations, chains, parameters 
# a <- extract(fit2, permuted = FALSE) 
# 
# ### use S3 functions as.array (or as.matrix) on stanfit objects
# a2 <- as.array(fit2)
# m <- as.matrix(fit2)
# 
# print(fit, digits = 1)
# 
# schools_dat <- read_rdump('8schools.rdump')
# 
# source('8schools.rdump') 
# fit <- stan(file = '8schools.rstan', data = c("J", "y", "sigma"), 
#             iter = 1000, chains = 4) 
# print(fit)
# plot(fit)



###
# R Sampling Functions
###

normalPD <- function(size)
{
  x = rnorm(size)
  
  # normalize
  min.x = min(x)
  max.x = max(x)
  
  x.norm = (x - min.x)/(max.x - min.x)
  return(x.norm)
}

Gini <- function(x,y,plot=F) 
{
  if(length(x)!=length(y)){return("Use vector of the same length.")};
  Data<-data.frame(x,y);
  if(dim(Data)[2]!=2){return("Use 1-column vectors only.")};
  colnames(Data)<-c("PD", "Flag");
  
  Data<-Data[order(-Data$PD),];
  N<-length(Data$PD);
  rownames(Data)<-c(1:N);
  interval<-1/N;
  
  Data$Bad<-Data$Flag/sum(Data$Flag);
  Data$Good<-(1-Data$Flag)/sum(1-Data$Flag);
  
  Data$cumBad<-cumsum(Data$Bad);
  Data$cumGood<-cumsum(Data$Good);
  Data$area<-Data$cumBad*interval;
  
  if(plot)
  {     
    df <- data.frame(a=Data$cumGood,b=Data$area/Data$area[N]) 
    ggp <- ggplot(df, aes(a, b)) + geom_line()+labs(y ="Ascending Cumulative Bad %", x="Ascending Cumulative Good %", main="Post overridden ROC curve")
#     Print.Plot(ggp, 'output/giniplot_post', 5, 5)
    print(ggp)
  } 
  Gini<-2*sum(Data$area)-1;
  return(Gini)  
}


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
  dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:reps), include.lowest = TRUE, labels = FALSE))
  dat$NewID <- factor(dat$NewID)
  return(dat)
}


DataGen <- function(s = 30000, r = 5000, c = 10, dg = 10, multi = 0.85)
{
  x = seq(from = 0.01, to = 1, by = 0.001)
  scale = seq(from = min(log(x)), to = max(log(x)), by = (max(log(x)) - min(log(x)))/20)
  
  masterScale <- data.frame(
    'Ranking' = seq_along(scale), 
    'Min' = c(0, round(exp(scale),4)[-length(scale)]), 
    'Max' = round(exp(scale),4), 
    'Mid' = c(0, round(exp(scale),4)[-length(scale)])*0.5 + round(exp(scale),4)*0.5
    )
  
  x = sample(x = seq(from = 1, to = r, by = 1),
             size = s,
             replace = TRUE)
  #   summary(x)
  #   length(x)
  #   hist(x)
  
  y = sample(x = seq(from = 1, to = c, by = 1),
             size = s,
             replace = TRUE)
  #   summary(y)
  #   length(y)
  #   hist(y)
  
  z = sample(x = seq(from = 1, to = dg, by = 1),
             size = s,
             replace = TRUE,
             prob = dnorm(seq(from = 1, to = dg, by = 1), mean = multi * dg, sd = 1, log = FALSE))
  #   summary(z)
  #   length(z)
  #   hist(z)
  
  v = sample(x = seq(from = 10000, to = 10000000, by = 10000),
             size = s,
             replace = TRUE)
  #     summary(v)
  #     length(v)
  #     hist(v)
  
  w = sample(x = c(0, 1),
             size = s,
             replace = TRUE,
             prob = c(0.9, 0.1))
  #     summary(w)
  #     length(w)
  #     hist(w)
             
  
  dataSet <- data.frame('RID' = x,
                        'CID' = y,
                        'Ranking' = z,
                        'CMid' = as.numeric(masterScale[match(z, masterScale$Ranking), 'Mid']),
                        'CMin' = as.numeric(masterScale[match(z, masterScale$Ranking), 'Min']),
                        'CMax' = as.numeric(masterScale[match(z, masterScale$Ranking), 'Max']),
                        'C_Value' = v,
                        'rD' = w)
  
  #   summary(dataSet)
  #   dim(dataSet)
  
  dataSet <- within(dataSet, {
    C_Def <- 0
  })
  
  dataSet$CPD <- round(runif(dim(dataSet)[1], min = dataSet$CMin, max = dataSet$CMax), 4)
  dataSet$rPD <- normalPD(dataSet$CPD)
  
#   plot(density(pnorm(runif(dim(dataSet)[1], -10, 10))))
#   plot(density(punif(rnorm(dataSet$CPD))))
  
  dataSet$C_Def[dataSet$Ranking == dg] <- 1
  dataSet <- dataSet[with(dataSet,order(RID, CID, CPD)),]
  #   length(unique(dataSet$RID))
  #   dim(dataSet[duplicated(paste0(dataSet$RID, '+', dataSet$CID))==TRUE,])
  dataSet <- dataSet[duplicated(paste0(dataSet$RID, '+', dataSet$CID))==FALSE,]
  #   dim(dataSet[duplicated(paste0(dataSet$RID, '+', dataSet$CID))==TRUE,])
  #   summary(dataSet)
  #   dim(dataSet)

  
  ###
  # Initialize parameters
  ###
  detectCores()
  workers <- makeCluster(detectCores())
  registerDoParallel(workers)
  getDoParWorkers()
  # data step
  retval <- ddply( .data=dataSet,
                   .variables=c('RID'),
                   .fun = function(subdata)
                   {
                     subdata$R_Def = rep(max(subdata$C_Def), length(subdata$C_Def))
                     subdata$RPD = 1 - prod(1 - subdata$CPD)
                     subdata$R_Value = sum(subdata$C_Value)
                     subdata$R_LGD = sum(subdata$C_Def*subdata$C_Value)/sum(subdata$C_Value)
                     subdata$R_Loss = sum(subdata$C_Def*subdata$C_Value)
                     return(subdata)
                   },
                   .parallel = TRUE
  )
  ###
  # Clean up parallel settings
  ###
  stopCluster(workers)
  
  return(retval)
}

###
# Data Gen
###
dataSet <- DataGen(multi = 0.82)
# summary(dataSet)
# dim(dataSet)

dataSet$ID <- paste0(dataSet$RID, ".", dataSet$CID)
dataSet$ID <- as.numeric(as.character(dataSet$ID))
dataSet <- dataSet[duplicated(dataSet$ID)==FALSE,]
dataSet <- dataSet[order(dataSet$ID), ]
summary(dataSet)
dim(dataSet)
head(dataSet)

Gini(dataSet$rPD,dataSet$rD)

test.df <- sampler(dataSet, "ID", reps = 10)

###
# Test Functions
###
gini = function(data, xname, yname, decreasing=FALSE, ordered=FALSE, boot.n=0)
{
  if (!ordered)
  {
    data = data.frame(
      "x" = factor(data[,xname], levels = sort(unique(data[,xname]),decreasing=decreasing)),
      #   "y" = factor(as.numeric(data[,yname]), levels = c(0,1)))
      "y" = factor(data[,yname], levels = c(0,1)))
    data = data[order(data$x),]
  }
  
  n = nrow(data)
  
  tt = table(data$x,data$y)
  t1 = rowSums(tt)
  t2 = colSums(tt)
  
  if (any(t2==0))
  {
    curve = data.frame(
      "x" = seq(0,1,1/n),
      "y" = seq(0,1,1/n))
  } else {
    curve = data.frame(
      "x" = c(0,cumsum(rep(tt[,1]/t2[1]/t1,times = t1))),
      "y" = c(0,cumsum(rep(tt[,2]/t2[2]/t1,times = t1))))
  }
  
  coeff = sum((curve$x[-1]-curve$x[-(n+1)])*(curve$y[-1] + curve$y[-(n+1)]))-1
  
  boot = NULL
  if (boot.n>0)
  {
    boot.coeff = data.frame("sample"=as.integer(), "value"=as.numeric())
    boot.curve = data.frame("sample"=as.integer(), curve[0,])
    
    for (b in seq(1,boot.n))
    {
      key = sort.int(sample.int(nrow(data),replace=TRUE))
      
      temp = gini(data[key,], "x", "y", ordered=TRUE)
      boot.coeff = rbind(boot.coeff, cbind("sample"=b, "value"=temp$coeff))
      boot.curve = rbind(boot.curve, cbind("sample"=b, temp$curve))
    }
    boot$n = boot.n
    boot$coeff = boot.coeff
    boot$curve = boot.curve
  }
  
  output = list(
    "coeff" = coeff,
    "curve" = curve,
    "boot"  = boot)
  
  class(output) = "gini"
  
  return(output)
  
}

####################################################################################################
# Plot Gini
###################################################################################################

plot.gini = function(gini,quantiles=c(0.05,0.95))
{
  if (class(gini)!="gini") return("Object not of class 'gini'.")
  
  require(ggplot2)
  
  p = ggplot(aes(x=x, y=y), data=gini$curve)
  p = p + 
    geom_line(colour="black", size=1) +
    geom_line(data=data.frame("x"=c(0,1), "y"=c(0,1)), aes(x=x, y=y), colour="blue", size=1, linetype=2) + 
    coord_fixed() + 
    xlab("Cumulative % non-defaulters") + ylab("Cumulative % defaulters") + 
    labs(title=substitute(paste("Gini coeff: ",number),list(number = round(gini$coeff,3))))
  
  if (!is.null(gini$boot))
  {
    n = min(gini$boot$n/100,20)
    
    data = gini$boot$curve
    data = data[order(data$x),]
    
    data$cut = cut(data$x, breaks=quantile(data$x,seq(0,n)/n), labels=seq(1,n))
    
    data = data[!is.na(data$cut),]
    
    x.q = rbind(c(0,0),melt(by(data, list(data$cut), function(data) {mean(data$x)})[])); names(x.q)[names(x.q)=="value"]="x"
    
    
    subp = ggplot(aes(x=value), data=gini$boot$coeff)
    subp = subp + 
      scale_y_continuous(breaks=NULL) +
      geom_density() + 
      geom_vline(xintercept = quantile(gini$coeff), colour="black")
    
    for (q in quantiles)
    {
      y.q = rbind(c(0,0),melt(by(data, list(data$cut), function(data) {quantile(data$y,q)})[])); names(y.q)[names(y.q)=="value"]="y"
      
      p = p + geom_line(data=merge(x.q,y.q,by="Var1"), aes(x=x, y=y), colour="red")
      subp = subp + geom_vline(xintercept=quantile(gini$boot$coeff$value,q), colour="red")
    }
    
    subp = subp +  
      xlab("Gini") + ylab("") + theme_bw()
    
    attr(p,"subplot") = subp
  }
  
  p = p + theme_bw();
  
  return(p);
  
}


dat = dataSet[,c('CPD', 'rD')]
dat$CPD = normalPD(dat$CPD)
dat$rD = sample(x = c(0, 1),
                size = dim(dat)[1],
                replace = TRUE,
                prob = c(0.9, 0.1))

x <- gini(dat, "CPD", "rD", decreasing=T, boot.n = 500)
summary(x)
plot.gini(x)



