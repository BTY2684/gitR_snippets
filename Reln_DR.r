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
pkgImport("parallel")
pkgImport("stats")
pkgImport("xts")

###
# Generate data set
###

x = seq(from = 0.01, to = 1, by = 0.001)
scale = seq(from = min(log(x)), to = max(log(x)), by = (max(log(x)) - min(log(x)))/9)

masterScale <- data.frame('Ranking' = seq_along(scale), 'PD' = round(exp(scale),4))

x = sample(x = seq(from = 1, to = 5000, by = 1),
           size = 30000,
           replace = TRUE)
summary(x)
length(x)
hist(x)

y = sample(x = seq(from = 1, to = 10, by = 1),
           size = 30000,
           replace = TRUE)
summary(y)
length(y)
hist(y)

z = sample(x = seq(from = 1, to = 10, by = 1),
           size = 30000,
           replace = TRUE)
summary(z)
length(z)
hist(z)

dataSet <- data.frame('RID' = x,
                      'CID' = y,
                      'Ranking' = z,
                      'CPD' = as.numeric(masterScale[match(z, masterScale$Ranking), 'PD']))
summary(dataSet)
dim(dataSet)

dataSet <- within(dataSet, {
  C_Def <- 0
})

dataSet$C_Def[dataSet$CPD == 1.0] <- 1
dataSet <- dataSet[with(dataSet,order(RID, CID, CPD)),]
length(unique(dataSet$RID))
dim(dataSet[duplicated(paste0(dataSet$RID, '+', dataSet$CID))==TRUE,])
dataSet <- dataSet[duplicated(paste0(dataSet$RID, '+', dataSet$CID))==FALSE,]
dim(dataSet[duplicated(paste0(dataSet$RID, '+', dataSet$CID))==TRUE,])
summary(dataSet)
dim(dataSet)


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
                   subdata$RPD = 1 - prod(1-subdata$CPD)
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

retval[retval$R_Def==1,][1:20,]
retval[1:20,]

###
# Output default rates on Customer, Cust_Relation, Relationship
###
mean(retval$C_Def)
mean(retval$R_Def)
mean(retval[with(retval, unique(RID)),'R_Def'])

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

ggplot(retval, aes(x = Ranking, y = RPD, colour = R_Def)) +
  geom_line(alpha = 1) + facet_grid(. ~ CID)

ggplot(retval, aes(x = factor(Ranking), y = RPD, colour = factor(R_Def))) +
  geom_line(alpha = .1) + facet_grid(factor(CID) ~ .)

###
# default vs non-default Hist plot
# on Customer, Cust_Relation, Relationship
###
hist(retval$CPD)
hist(retval$RPD)
hist(retval[with(retval, unique(RID)),'RPD'])
