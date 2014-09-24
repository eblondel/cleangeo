# clgeo_SummaryReport.R
# ------------------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2014-09-24
#
clgeo_SummaryReport <- function(report){
  return(summary(report[,-c(4,5)]))
}