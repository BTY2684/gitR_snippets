(WD <- getwd())

###
# Included libraries
###
pkgImport("LearnBayes")
pkgImport("parallel")


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
  dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:reps), include.lowest = TRUE,
                              labels = FALSE))
  dat$NewID <- factor(dat$NewID)
  return(dat)
}


DataGen <- function(s = 30000, r = 5000, c = 10, dg = 10, multi = 0.85)
{
  x = seq(from = 0.01, to = 1, by = 0.001)
  scale = seq(from = min(log(x)), to = max(log(x)), by = (max(log(x)) - min(log(x)))/9)
  
  masterScale <- data.frame('Ranking' = seq_along(scale), 'PD' = round(exp(scale),4))
  
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
  
  dataSet <- data.frame('RID' = x,
                        'CID' = y,
                        'Ranking' = z,
                        'CPD' = as.numeric(masterScale[match(z, masterScale$Ranking), 'PD']),
                        'C_Value' = v)
  
  #   summary(dataSet)
  #   dim(dataSet)
  
  dataSet <- within(dataSet, {
    C_Def <- 0
  })
  
  dataSet$C_Def[dataSet$CPD == 1.0] <- 1
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
# summary(dataSet)
# dim(dataSet)

Gini(dataSet$RPD,dataSet$R_Def)
