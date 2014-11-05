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

data.df$ID <- paste0(data.df$RID, ".", data.df$CustNum)

startDate = 541
endDate = 590
burnIn = 15
obsPeriod = seq(from = startDate - burnIn,
                to = endDate,
                by = 1)

ts.df <- data.frame('ID' = data.df$ID[data.df$iter == startDate - burnIn],
                    data.df$CPD[data.df$iter == startDate - burnIn])
for (j in obsPeriod[-1]) {
  tmp.df <- data.frame('ID' = data.df$ID[data.df$iter == j],
                       data.df$CPD[data.df$iter == j])
  ts.df <- merge(ts.df, tmp.df, by.x = "ID", by.y = "ID", all = TRUE)
}

names(ts.df) <- c('ID', paste0('Date', seq_along(obsPeriod)))
ts.df$ID <- as.numeric(as.character(ts.df$ID))
ts.df <- ts.df[order(ts.df$ID), ]

ts.m <- as.matrix(ts.df[, -1])

ts.m[ts.m > 0.8] = 'D'
ts.m[ts.m <= 0.8] = 'L'
ts.m[is.na(ts.m)] = 0

ts.df1 <- data.frame('ID' = ts.df$ID, ts.m)
head(ts.df1)

keys = rev(names(ts.df1)[-1])[1:15]
tmp.df <- ts.df1[, c('ID', keys)]
for (j in seq_along(keys) ) {
  levels(tmp.df[,names(tmp.df)%in%keys[j]]) = c(levels(tmp.df[,names(tmp.df)%in%keys[j]]), 'l', 'd')
}

backFill <- function(data, key)
{
  ###
  # backward info fill for 15 month
  ###

  tmp.df <- data
  keys <- key

  if(length(keys)>=2)
  {
    ### latest month missing where -1 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[2-1]] == 0 & tmp.df[, names(tmp.df)%in%keys[2]] == 'L', names(tmp.df)%in%keys[2-1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[2-1]] == 0 & tmp.df[, names(tmp.df)%in%keys[2]] == 'D', names(tmp.df)%in%keys[2-1]] = 'd'
  }

  if(length(keys)>=3)
  {
    ### latest & -1 month missing where -2 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[3-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[3-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[3]] == 'L',
           names(tmp.df)%in%keys[2:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[3-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[3-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[3]] == 'D',
           names(tmp.df)%in%keys[2:1]] = 'd'
  }

  if(length(keys)>=4)
  {
    ### latest & -1 & -2 month missing where -3 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[4-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4]] == 'L',
           names(tmp.df)%in%keys[3:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[4-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4]] == 'D',
           names(tmp.df)%in%keys[3:1]] = 'd'
  }

  if(length(keys)>=5)
  {
    ### latest & -1 & -2 & -3 month missing where -4 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[5-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5]] == 'L',
           names(tmp.df)%in%keys[4:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[5-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5]] == 'D',
           names(tmp.df)%in%keys[4:1]] = 'd'
  }

  if(length(keys)>=6)
  {
    ### latest & -1 & -2 & -3 & -4 month missing where -5 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[6-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6]] == 'L',
           names(tmp.df)%in%keys[5:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[6-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6]] == 'D',
           names(tmp.df)%in%keys[5:1]] = 'd'
  }

  if(length(keys)>=7)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 month missing where -6 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[7-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7]] == 'L',
           names(tmp.df)%in%keys[6:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[7-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7]] == 'D',
           names(tmp.df)%in%keys[6:1]] = 'd'
  }

  if(length(keys)>=8)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 month missing where -7 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[8-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8]] == 'L',
           names(tmp.df)%in%keys[7:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[8-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8]] == 'D',
           names(tmp.df)%in%keys[7:1]] = 'd'
  }

  if(length(keys)>=9)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 month missing where -8 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[9-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9]] == 'L',
           names(tmp.df)%in%keys[8:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[9-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9]] == 'D',
           names(tmp.df)%in%keys[8:1]] = 'd'
  }

  if(length(keys)>=10)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 month missing where -9 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[10-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10]] == 'L',
           names(tmp.df)%in%keys[9:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[10-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10]] == 'D',
           names(tmp.df)%in%keys[9:1]] = 'd'
  }

  if(length(keys)>=11)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 month missing where -10 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[11-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11]] == 'L',
           names(tmp.df)%in%keys[10:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[11-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11]] == 'D',
           names(tmp.df)%in%keys[10:1]] = 'd'
  }

  if(length(keys)>=12)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 & -10 month missing where -11 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[12-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12]] == 'L',
           names(tmp.df)%in%keys[11:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[12-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12]] == 'D',
           names(tmp.df)%in%keys[11:1]] = 'd'
  }

  if(length(keys)>=13)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 & -10 & -11 month missing where -12 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[13-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13]] == 'L',
           names(tmp.df)%in%keys[12:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[13-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13]] == 'D',
           names(tmp.df)%in%keys[12:1]] = 'd'
  }

  if(length(keys)>=14)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 & -10 & -11 & -12 month missing where -13 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[14-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-13]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14]] == 'L',
           names(tmp.df)%in%keys[13:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[14-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-13]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14]] == 'D',
           names(tmp.df)%in%keys[13:1]] = 'd'
  }

  if(length(keys)>=15)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 & -10 & -11 & -12 & -13 month missing where -14 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[15-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-13]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-14]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15]] == 'L',
           names(tmp.df)%in%keys[14:1]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[15-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-13]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-14]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15]] == 'D',
           names(tmp.df)%in%keys[14:1]] = 'd'
  }

  return(tmp.df)
}

tmp.df <- backFill(tmp.df, keys)

na.omit(tmp.df)[1:20,]

###
# gap between two Obs within 15 month
###

gapFill <- function(data, key)
{
  tmp.df <- data
  keys <- key
  if(length(key) >= 3)
  {
    ### latest & -1 month missing where -2 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[3-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[3-2]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[3]] == 'L',
           names(tmp.df)%in%keys[2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[3-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[3-2]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[3]] == 'D',
           names(tmp.df)%in%keys[2]] = 'd'
  }

  if(length(key) >= 4)
  {
    ### latest & -1 & -2 month missing where -3 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[4-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4-3]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[4]] == 'L',
           names(tmp.df)%in%keys[3:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[4-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4-3]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[4]] == 'D',
           names(tmp.df)%in%keys[3:2]] = 'd'
  }

  if(length(key) >= 5)
  {
    ### latest & -1 & -2 & -3 month missing where -4 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[5-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-4]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[5]] == 'L',
           names(tmp.df)%in%keys[4:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[5-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-4]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[5]] == 'D',
           names(tmp.df)%in%keys[4:2]] = 'd'
  }

  if(length(key) >= 6)
  {
    ### latest & -1 & -2 & -3 & -4 month missing where -5 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[6-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-5]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[6]] == 'L',
           names(tmp.df)%in%keys[5:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[6-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-5]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[6]] == 'D',
           names(tmp.df)%in%keys[5:2]] = 'd'
  }

  if(length(key) >= 7)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 month missing where -6 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[7-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-6]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[7]] == 'L',
           names(tmp.df)%in%keys[6:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[7-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-6]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[7]] == 'D',
           names(tmp.df)%in%keys[6:2]] = 'd'
  }

  if(length(key) >= 8)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 month missing where -7 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[8-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-7]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[8]] == 'L',
           names(tmp.df)%in%keys[7:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[8-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-7]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[8]] == 'D',
           names(tmp.df)%in%keys[7:2]] = 'd'
  }

  if(length(key) >= 9)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 month missing where -8 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[9-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-8]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[9]] == 'L',
           names(tmp.df)%in%keys[8:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[9-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-8]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[9]] == 'D',
           names(tmp.df)%in%keys[8:2]] = 'd'
  }

  if(length(key) >= 10)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 month missing where -9 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[10-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-9]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[10]] == 'L',
           names(tmp.df)%in%keys[9:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[10-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-9]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[10]] == 'D',
           names(tmp.df)%in%keys[9:2]] = 'd'
  }

  if(length(key) >= 11)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 month missing where -10 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[11-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-10]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[11]] == 'L',
           names(tmp.df)%in%keys[10:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[11-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-10]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[11]] == 'D',
           names(tmp.df)%in%keys[10:2]] = 'd'
  }

  if(length(key) >= 12)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 & -10 month missing where -11 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[12-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-11]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[12]] == 'L',
           names(tmp.df)%in%keys[11:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[12-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-11]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[12]] == 'D',
           names(tmp.df)%in%keys[11:2]] = 'd'
  }

  if(length(key) >= 13)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 & -10 & -11 month missing where -12 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[13-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-12]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[13]] == 'L',
           names(tmp.df)%in%keys[12:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[13-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-12]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[13]] == 'D',
           names(tmp.df)%in%keys[12:2]] = 'd'
  }

  if(length(key) >= 14)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 & -10 & -11 & -12 month missing where -13 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[14-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-13]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[14]] == 'L',
           names(tmp.df)%in%keys[13:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[14-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-13]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[14]] == 'D',
           names(tmp.df)%in%keys[13:2]] = 'd'
  }

  if(length(key) >= 15)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 & -10 & -11 & -12 & -13 month missing where -14 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[15-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-13]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-14]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[15]] == 'L',
           names(tmp.df)%in%keys[14:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[15-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-13]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-14]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[15]] == 'D',
           names(tmp.df)%in%keys[14:2]] = 'd'
  }

return(tmp.df)
}

keys = names(tmp.df)[-1]

for (i in seq_along(keys[-1]))
{
  tmp.df <- gapFill(tmp.df, keys)

  keys <- keys[-1]
}

tmp.df <- tmp.df[tmp.df$Date65!=0,]
na.omit(tmp.df)[1:20,]

changeFill <- function(data, key)
{
  tmp.df <- data
  keys <- key
  if(length(key) >= 3)
  {
    ### latest & -1 month missing where -2 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[3-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[3-2]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[3]] == 'L',
           names(tmp.df)%in%keys[2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[3-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[3-2]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[3]] == 'D',
           names(tmp.df)%in%keys[2]] = 'd'
  }

  if(length(key) >= 4)
  {
    ### latest & -1 & -2 month missing where -3 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[4-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4-3]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[4]] == 'L',
           names(tmp.df)%in%keys[3:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[4-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[4-3]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[4]] == 'D',
           names(tmp.df)%in%keys[3:2]] = 'd'
  }

  if(length(key) >= 5)
  {
    ### latest & -1 & -2 & -3 month missing where -4 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[5-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-4]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[5]] == 'L',
           names(tmp.df)%in%keys[4:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[5-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[5-4]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[5]] == 'D',
           names(tmp.df)%in%keys[4:2]] = 'd'
  }

  if(length(key) >= 6)
  {
    ### latest & -1 & -2 & -3 & -4 month missing where -5 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[6-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-5]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[6]] == 'L',
           names(tmp.df)%in%keys[5:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[6-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[6-5]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[6]] == 'D',
           names(tmp.df)%in%keys[5:2]] = 'd'
  }

  if(length(key) >= 7)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 month missing where -6 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[7-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-6]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[7]] == 'L',
           names(tmp.df)%in%keys[6:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[7-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[7-6]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[7]] == 'D',
           names(tmp.df)%in%keys[6:2]] = 'd'
  }

  if(length(key) >= 8)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 month missing where -7 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[8-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-7]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[8]] == 'L',
           names(tmp.df)%in%keys[7:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[8-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[8-7]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[8]] == 'D',
           names(tmp.df)%in%keys[7:2]] = 'd'
  }

  if(length(key) >= 9)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 month missing where -8 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[9-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-8]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[9]] == 'L',
           names(tmp.df)%in%keys[8:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[9-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[9-8]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[9]] == 'D',
           names(tmp.df)%in%keys[8:2]] = 'd'
  }

  if(length(key) >= 10)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 month missing where -9 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[10-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-9]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[10]] == 'L',
           names(tmp.df)%in%keys[9:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[10-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[10-9]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[10]] == 'D',
           names(tmp.df)%in%keys[9:2]] = 'd'
  }

  if(length(key) >= 11)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 month missing where -10 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[11-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-10]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[11]] == 'L',
           names(tmp.df)%in%keys[10:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[11-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[11-10]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[11]] == 'D',
           names(tmp.df)%in%keys[10:2]] = 'd'
  }

  if(length(key) >= 12)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 & -10 month missing where -11 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[12-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-11]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[12]] == 'L',
           names(tmp.df)%in%keys[11:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[12-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[12-11]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[12]] == 'D',
           names(tmp.df)%in%keys[11:2]] = 'd'
  }

  if(length(key) >= 13)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 & -10 & -11 month missing where -12 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[13-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-12]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[13]] == 'L',
           names(tmp.df)%in%keys[12:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[13-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[13-12]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[13]] == 'D',
           names(tmp.df)%in%keys[12:2]] = 'd'
  }

  if(length(key) >= 14)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 & -10 & -11 & -12 month missing where -13 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[14-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-13]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[14]] == 'L',
           names(tmp.df)%in%keys[13:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[14-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[14-13]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[14]] == 'D',
           names(tmp.df)%in%keys[13:2]] = 'd'
  }

  if(length(key) >= 15)
  {
    ### latest & -1 & -2 & -3 & -4 & -5 & -6 & -7 & -8 & -9 & -10 & -11 & -12 & -13 month missing where -14 month is not
    tmp.df[tmp.df[, names(tmp.df)%in%keys[15-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-13]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-14]] == 'D' &
             tmp.df[, names(tmp.df)%in%keys[15]] == 'L',
           names(tmp.df)%in%keys[14:2]] = 'l'
    tmp.df[tmp.df[, names(tmp.df)%in%keys[15-1]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-2]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-3]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-4]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-5]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-6]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-7]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-8]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-9]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-10]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-11]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-12]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-13]] == 0 &
             tmp.df[, names(tmp.df)%in%keys[15-14]] == 'L' &
             tmp.df[, names(tmp.df)%in%keys[15]] == 'D',
           names(tmp.df)%in%keys[14:2]] = 'd'
  }

  return(tmp.df)
}

keys = names(tmp.df)[-1]

for (i in seq_along(keys[-1]))
{
  tmp.df <- changeFill(tmp.df, keys)

  keys <- keys[-1]
}

tmp.df <- tmp.df[tmp.df$Date65!=0,]
na.omit(tmp.df)[1:20,]









head(na.omit(tmp.df))
head(na.omit(tmp.df[tmp.df$Date65=='D',]))





tmp.df <- ts.df1[, c(rev(names(ts.df1)[-1])[2:1])]
tmp.df[, c(rev(names(ts.df1)[-1])[1:1])]



dim(tmp.df)
head(tmp.df)
tail(tmp.df)


### clean up

data.df1 <- read.csv("Z:/tempData/wholePeriod_obs.csv")
data.df1[1:20,]

for(i in seq(2, 89, 1))
{
  for(j in seq(0, 11, 1))
  {
    data.df1[, i+j] = as.character(data.df1[, i+j])
    keys = which(!(data.df1[, i+j]%in%c('l', 'd', 'L', 'D', 0)))
    data.df1[keys, i+j] = 0
  }
}

data.df1[1:20,]
class(data.df1[, 3])

write.csv(data.df1, "Z:/tempData/whole_obs.csv")

### loop

data.df <- read.csv("Z:/tempData/wholePeriod_defaultUpdate.csv")
head(tmp.df[,2:13])
head(tmp.df[,89:100])

data.df[1:20,]

for(i in seq(2, 89, 1))
{
  for(j in seq(0, 11, 1))
  {
    data.df[, i+j] = as.character(data.df[, i+j])
    keys = which(!(data.df[, i+j]%in%c('l', 'd', 'L', 'D', 0)))
    data.df[keys, i+j] = 0
  }
}

data.df[1:20,]
class(data.df[, 3])

tmp.df <- data.df
tmp.df[tmp.df$ID==108.3,]

for(i in seq(2, 89, 1))
{
  for(j in seq(1, 11, 1))
  {
    keys = which(substr(tmp.df[, i], 1, 1)=='D' &
                   substr(tmp.df[, i + j], 1, 1)=='L')
    for(k in seq(i + j, i + 11, 1))
    {
      tmp.df[keys, k] = paste0(substr(tmp.df[keys, k], 1, 1), '/d')
    }
  }
}

tmp.df[tmp.df$ID==108.3,]
tmp.df[tmp.df$ID==3510.2,]

write.csv(tmp.df, "Z:/tempData/whole_defaultUpdate.csv")

