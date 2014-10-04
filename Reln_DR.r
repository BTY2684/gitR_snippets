###
# Included libraries
###
pkgImport("utils")
pkgImport("parallel")
pkgImport("foreach")
pkgImport("doParallel")
pkgImport("McSpatial")
pkgImport("ggplot2")
pkgImport("reshape2")
pkgImport("plyr")
pkgImport("iterators")
pkgImport("foreach")
pkgImport("doParallel")
pkgImport("stats")
pkgImport("xts")
pkgImport("doSNOW")

# NOTE: From version 0.3-8 onwards snow includes this behavior
# and thus this custom version of clusterExport is no longer needed
clusterExport <- local({
  gets <- function(n, v) { assign(n, v, envir = .GlobalEnv); NULL }
  function(cl, list, envir = .GlobalEnv) {
    ## do this with only one clusterCall--loop on slaves?
    for (name in list) {
      clusterCall(cl, gets, name, get(name, envir = envir))
    }
  }
})

# Functions
createCluster = function(noCores, logfile = "/dev/null", export = NULL, lib = NULL) {
  require(doSNOW)
  cl <- makeCluster(noCores, type = "SOCK", outfile = logfile)
  if(!is.null(export)) clusterExport(cl, export)
  if(!is.null(lib)) {
    l_ply(lib, function(dum) {
      clusterExport(cl, "dum", envir = environment())
      clusterEvalQ(cl, library(dum, character.only = TRUE))
    })
  }
  registerDoSNOW(cl)
  return(cl)
}

###
# Generate data set
###
samplerRun <- function()
{
  require(parallel)
  require(plyr)
  require(iterators)
  require(foreach)
  require(doParallel)

  x = seq(from = 0.01, to = 1, by = 0.001)
  scale = seq(from = min(log(x)), to = max(log(x)), by = (max(log(x)) - min(log(x)))/9)

  masterScale <- data.frame('Ranking' = seq_along(scale), 'PD' = round(exp(scale),4))

  x = sample(x = seq(from = 1, to = 5000, by = 1),
             size = 30000,
             replace = TRUE)
  #   summary(x)
  #   length(x)
  #   hist(x)

  y = sample(x = seq(from = 1, to = 10, by = 1),
             size = 30000,
             replace = TRUE)
  #   summary(y)
  #   length(y)
  #   hist(y)

  z = sample(x = seq(from = 1, to = 10, by = 1),
             size = 30000,
             replace = TRUE,
             prob = dnorm(seq(from = 1, to = 10, by = 1), mean = 7, sd = 1, log = FALSE))
  #   summary(z)
  #   length(z)
  #   hist(z)

  v = sample(x = seq(from = 10000, to = 10000000, by = 10000),
             size = 30000,
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

  #   head(retval)
  #   summary(retval)
  #   dim(retval)
  #
  #   retval[retval$R_Def==1,][1:20,]
  #   retval[1:20,]

  ###
  # Output default rates on Customer, Cust_Relation, Relationship
  ###
  #   mean(retval$C_Def)
  #   mean(retval$R_Def)
  #   mean(retval[with(retval, unique(RID)),'R_Def'])
  return(c(mean(retval$C_Def),
           mean(retval$R_Def),
           mean(retval[duplicated(retval$RID)==FALSE,'R_Def']),
           mean(retval$CPD),
           mean(retval$RPD),
           mean(retval[duplicated(retval$RID)==FALSE,'RPD']),

#            cust PD = mean(retval$C_Def)
#            cust LGD = 1
#            cust EAD = sum(retval$C_Def*retval$C_Value)
           mean(retval$C_Def)*sum(retval$C_Def*retval$C_Value),

#            reln_adj_cust PD = mean(retval$R_Def)
#            reln_adj_cust LGD = sum(retval$R_Loss)/sum(retval$R_Def*retval$R_Value)
#            reln_adj_cust EAD = sum(retval$C_Def*retval$R_Value*retval$R_LGD)

           mean(retval$R_Def)*sum(retval$R_Loss)/sum(retval$R_Def*retval$R_Value)*sum(retval$C_Def*retval$R_Value*retval$R_LGD),

#            reln PD = mean(retval[duplicated(retval$RID)==FALSE,'R_Def'])
#            reln LGD = sum(retval[duplicated(retval$RID)==FALSE,'R_Loss'])/sum(retval[duplicated(retval$RID)==FALSE,'R_Value']*retval[duplicated(retval$RID)==FALSE,'R_Def'])
#            reln EAD = sum(retval[duplicated(retval$RID)==FALSE,'R_Value']*retval[duplicated(retval$RID)==FALSE,'R_LGD'])

           mean(retval[duplicated(retval$RID)==FALSE,'R_Def'])*sum(retval[duplicated(retval$RID)==FALSE,'R_Loss'])/sum(retval[duplicated(retval$RID)==FALSE,'R_Value']*retval[duplicated(retval$RID)==FALSE,'R_Def'])*sum(retval[duplicated(retval$RID)==FALSE,'R_Value']*retval[duplicated(retval$RID)==FALSE,'R_LGD'])
  ) )
}

# LGD is obtained by dividing total losses by the total amount of assets in default.

paraTast <- function(df, FUNC)
{
  require(parallel)
  require(plyr)
  require(iterators)
  require(foreach)
  require(doParallel)

  detectCores()
  workers <- createCluster(detectCores(), export = list(as.character(substitute(FUNC))), lib = list("parallel", "plyr", "iterators", "foreach", "doParallel"))
  registerDoParallel(workers)
  getDoParWorkers()

  retval <- ddply( .data=df,
                   .variables=names(df),
                   .fun = function(subdata)
                   {
                     subdata$ret = t(FUNC() )
                     return(subdata)
                   },
                   #                    .progress = progress_text(char = "-"),
                   .parallel = TRUE
  )

  stopCluster(workers)
  return(retval)
}

data.df <- data.frame('iter' = seq(1, 5000, 1))

system.time(a <- paraTast(data.df, samplerRun))
write.csv(a, file = "~/GitPro/tmpOutput/RelnDREL.csv", row.names = FALSE)

a <- read.csv(file = "~/GitPro/tmpOutput/RelnDREL.csv")

data.df$custDR <- a$ret.1
data.df$custRelnDR <- a$ret.2
data.df$relnDR <- a$ret.3
data.df$custPD <- a$ret.4
data.df$custRelnPD <- a$ret.5
data.df$relnPD <- a$ret.6
data.df$custEL <- a$ret.7
data.df$custRelnEL <- a$ret.8
data.df$relnEL <- a$ret.9

summary(data.df)

###
# Default Rate plot
# on Customer, Cust_Relation, Relationship
###

ggplot(data.df, aes(iter)) +
  geom_line(aes(y = custDR, colour = "Cust DR")) +
  geom_line(aes(y = mean(custDR), colour = "Mean Cust DR")) +
  geom_line(aes(y = custRelnDR, colour = "Reln Adjusted Cust DR")) +
  geom_line(aes(y = mean(custRelnDR), colour = "Mean Reln Adjusted Cust DR")) +
  geom_line(aes(y = relnDR, colour = "Reln DR")) +
  geom_line(aes(y = mean(relnDR), colour = "Mean Reln DR"), size = 1) +
  ggtitle("Cust vs Reln vs Cust Adjusted Reln Default Rates") +
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        legend.position = "bottom") +
  xlab("Senarios") +
  ylab("Default Rate") +
  scale_colour_discrete(name = "Colour Guide")

###
# Expected Loss plot
# on Customer, Cust_Relation, Relationship
###

ggplot(data.df, aes(iter)) +
  geom_line(aes(y = custEL, colour = "Cust EL")) +
  geom_line(aes(y = mean(custEL), colour = "Mean Cust EL")) +
  geom_line(aes(y = custRelnEL, colour = "Reln Adjusted Cust EL")) +
  geom_line(aes(y = mean(custRelnEL), colour = "Mean Reln Adjusted Cust EL")) +
  geom_line(aes(y = relnEL, colour = "Reln EL")) +
  geom_line(aes(y = mean(relnEL), colour = "Mean Reln EL"), size = 1) +
  ggtitle("Cust vs Reln vs Cust Adjusted Reln Expected Loss") +
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        legend.position = "bottom") +
  xlab("Senarios") +
  ylab("Expected Loss") +
  scale_colour_discrete(name = "Colour Guide")







###
# default vs non-default density plot
# on Customer, Cust_Relation, Relationship
###
plot(density(retval$C_Def))
plot(density(retval$R_Def))
plot(density(retval[with(retval, unique(RID)),'R_Def']))

###
# default vs non-default PD plot
# on Customer, Cust_Relation, Relationship
###
plot(density(retval$CPD))
plot(density(retval$RPD))
plot(density(retval[with(retval, unique(RID)),'RPD']))

ggplot(retval, aes(x = CPD)) + stat_density(alpha = 0.5, aes(fill=factor(-C_Def)), adjust = 1)

ggplot(retval, aes(x = RPD)) + stat_density(alpha = 0.5, aes(fill=factor(-R_Def)), adjust = 1)

ggplot(retval[with(retval, unique(RID)),], aes(x = RPD)) + stat_density(alpha = 0.5, aes(fill=factor(-R_Def)), adjust = 1)


###
# default vs non-default Hist plot
# on Customer, Cust_Relation, Relationship
###
hist(retval$CPD)
hist(retval$RPD)
hist(retval[with(retval, unique(RID)),'RPD'])

######################################################
# distribution test
######################################################
data.df <- data.frame('iter' = seq(1,5000,1))

###
# Initialize parameters
###
detectCores()
workers <- makeCluster(detectCores())
registerDoParallel(workers)
getDoParWorkers()
# data step
retval <- ddply( .data=data.df,
                 .variables=c('iter'),
                 .fun = function(subdata)
                 {
                   subdata$unifSum = sum(runif(1), runif(1))
                   subdata$unifProd = prod(runif(1), runif(1))
                   subdata$normalSum = sum(rnorm(1), rnorm(1))
                   subdata$normalProd = prod(rnorm(1), rnorm(1))
                   subdata$norm_unif_sum = sum(runif(1), rnorm(1))
                   subdata$norm_unif_prod = prod(runif(1), rnorm(1))
                   subdata$ratio = prod(rnorm(1), rnorm(1)) / sum(rnorm(1), rnorm(1))

                   return(subdata)
                 },
                 .parallel = TRUE
)
###
# Clean up parallel settings
###
stopCluster(workers)

head(retval)
summary(retval)
dim(retval)


ggplot(retval, aes(x = normalProd)) + stat_density(alpha = 0.5, adjust = 1)

test <- rnorm(5000)
## result : sum of uniform distribution is NOT 'Normal'
## Perform the test
ggplot(retval, aes(x = unifSum)) + stat_density(alpha = 0.5, adjust = 1)
shapiro.test(retval$unifSum)
ks.test(retval$unifSum, retval$ratio)
qqnorm(retval$unifSum);qqline(retval$unifSum, col = 2)

ggplot(retval, aes(x = unifProd)) + stat_density(alpha = 0.5, adjust = 1)
shapiro.test(retval$unifProd)
ks.test(retval$unifProd, retval$ratio)
qqnorm(retval$unifProd);qqline(retval$unifProd, col = 2)

ggplot(retval, aes(x = normalSum)) + stat_density(alpha = 0.5, adjust = 1)
shapiro.test(retval$normalSum)
ks.test(retval$normalSum, retval$ratio)
qqnorm(retval$normalSum);qqline(retval$normalSum, col = 2)

ggplot(retval, aes(x = norm_unif_sum)) + stat_density(alpha = 0.5, adjust = 1)
shapiro.test(retval$norm_unif_sum)
qqnorm(retval$norm_unif_sum);qqline(retval$norm_unif_sum, col = 2)

ggplot(retval, aes(x = norm_unif_prod)) + stat_density(alpha = 0.5, adjust = 1)
shapiro.test(retval$norm_unif_prod)
qqnorm(retval$norm_unif_prod);qqline(retval$norm_unif_prod, col = 2)

ggplot(retval, aes(x = ratio)) + stat_density(alpha = 0.5, adjust = 1)
shapiro.test(retval$ratio)
qqnorm(retval$ratio);qqline(retval$ratio, col = 2)



