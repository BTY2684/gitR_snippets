(WD <- getwd())

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
pkgImport("RSQLite")


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
# Data from database
###
drv <- dbDriver("SQLite")
con <- dbConnect(drv, "./DataBase/SQLite/simuDB.db")
dbListTables(con)
data.df = dbGetQuery(con, "Select distinct iter, RID, CustNum, RPD, CPD from reln_simu where iter >= 501 and iter <= 600")
summary(data.df)

###
# Data clean up
###
data.df$CustNum <- as.numeric(data.df$CustNum)
data.df$RPD <- round(as.numeric(data.df$RPD), 4)
data.df$CPD <- round(as.numeric(data.df$CPD), 4)
summary(data.df)

# ggplot(data.df, aes(iter)) +
#   geom_line(aes(y = RPD, colour = "RPD")) +
#   geom_line(aes(y = CPD, colour = "CPD")) +
#   theme(plot.title = element_text(lineheight=.8, face="bold"),
#         legend.position = "bottom") +
#   xlab("Iterations") + ylab("PD") +
#   scale_colour_discrete(name = "Colour Guide")

x = seq(from = 0.0001, to = 1, by = 0.0001)
y = seq(from = 0.1, to = 1000, by = 0.1)
scoreScale <- data.frame('PD' = x, 'Score' = y)
scale = seq(from = min(log(x)), to = max(log(x)), by = (max(log(x)) - min(log(x)))/20)
masterScale <- data.frame('Ranking' = seq_along(scale), 'PD' = round(exp(scale),4))
masterScale$PD[masterScale$Ranking==21] <- 0.95
masterScale <- rbind(masterScale, c('22', 1.5)) # 22 as default
masterScale

data.df$rating <- cut(data.df$RPD, breaks = c(0, masterScale$PD), right = FALSE, labels = masterScale$Ranking)
table(data.df$rating)
data.df$RScore <- round(scoreScale[match(data.df$RPD, scoreScale$PD), 'Score'],0)

length(unique(data.df[is.na(data.df$RScore),'RPD']))
scoreScale[scoreScale$PD==0.3843,]


model_logit <- glm(RPD ~ RScore, data = data.df, family = binomial(logit))
summary(model_logit)
confint(model_logit)
confint.default(model_logit)
head(fitted(model_logit))
head(model_logit$model)

model <- data.frame(model_logit$model, 'fitted' = fitted(model_logit))
head(model)

ggplot(model, aes(RScore)) +
  geom_line(aes(y = RPD, colour = "RPD")) +
  geom_line(aes(y = fitted, colour = "Fitted"))




tmp.A = ddply(data.df,.(iter, rating),summarise,Count=length(RPD))

addRatio = function(x){tmp = ddply(x,.(iter),summarise,Total=sum(Count));
                       x$Ratio = x$Count/tmp[match(as.character(x$iter),tmp$iter),"Total"];
                       return(x)}

RatioPlot = function(x,file=0){p <- ggplot(na.omit(x), aes(iter, Ratio, fill = rating));
                               p <- p + geom_histogram(aes(x = factor(iter), y= Ratio), alpha=1, position='stack', stat = 'identity');
                               p;
                               if(file!=0){pdf(paste('./Plots/',file,'.pdf',sep=''));
                                           print(p);
                                           dev.off()
                               }
}

RatingPlot = function(x,file=0){p <- ggplot(na.omit(x), aes(iter, Ratio, fill = rating));
                                p <- p + geom_histogram(aes(x = factor(iter), y= Ratio), alpha=1, position='stack', stat = 'identity');
                                p;
                                if(file!=0){pdf(paste('./Plots/',file,'.pdf',sep=''));
                                            print(p);
                                            dev.off()
                                }
}

RatingPlot(addRatio(tmp.A))
ggplot(addRatio(tmp.A), aes(iter, fill=rating)) + geom_bar()
##################################################################################

curve(dweibull(x, scale=13, shape=1.8),from=0, to=22, main="Weibull distribution")

testDist = sample(x = seq(from = 1, to = 22, by = 1),
                  size = 10000,
                  replace = TRUE,
                  prob = dweibull(seq(from = 1, to = 22, by = 1), scale=12, shape=1.8, log = FALSE))
testData <- data.frame('iter' = seq_along(testDist), 'PD' = testDist)
# testData$rating <- cut(testData$PD, breaks = c(0, masterScale$PD), right = FALSE, labels = masterScale$Ranking)
# table(testData$rating)
ggplot(testData, aes(x=testDist)) + geom_histogram()

ggplot(testData, aes(x=rating)) +
  geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
  geom_density(alpha=.2, aes(fill=rating))

cdf <- ddply(testData, "rating", summarise, pd.mean=mean(PD))

ggplot(testData, aes(x=rating)) +
  geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
  geom_density(alpha=.2, aes(fill=rating))
#   + geom_vline(data=cdf, aes(xintercept=pd.mean,  colour=rating), linetype="dashed", size=1)


