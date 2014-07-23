pkgImport <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgImport("ggplot2")
pkgImport("mvtnorm")
pkgImport("boot")
pkgImport("doParallel")
pkgImport("hexbin")
pkgImport("aod")
pkgImport("effects")

(WD <- getwd())
list.dirs(WD)

if (!is.null(WD)) setwd(WD)

################
# data clean up
################
# csv_data = read.csv('./gitProj/data_dump/randn_gg.csv', sep=',', header = TRUE)
# names(csv_data) <- c('group', 'type', 'desc', 'local', 'lp_desc', 'ccf', 'f_ccf', 'count', 'bal', 'exp')
# head(csv_data)
#
# key = as.numeric(csv_data$bal)==1
# csv_data$bal[key] = NA
# csv_data$bal <- as.numeric(csv_data$bal)
#
# key = as.numeric(csv_data$exp)==1
# csv_data$exp[key] = NA
# csv_data$exp <- as.numeric(csv_data$exp)
#
# write.csv(csv_data, './gitProj/data_dump/randn_gg_1.csv', row.names=FALSE, na="")

csv_data = read.csv('./gitProj/data_dump/randn_gg_1.csv', sep=',', header = TRUE)

csv_data_AODS = subset(csv_data, group = 'AODS')
head(csv_data_AODS)

p = ggplot(csv_data, aes(factor(group), log(count), color = f_ccf, size = 2)) + ylim(0, 5.5)
# p + geom_point() + facet_grid(ccf ~ . )
p + geom_jitter() + facet_grid(ccf ~ . )
