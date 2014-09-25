# clgeo_CollectionReport.R
# ------------------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2014-09-23
#
clgeo_CollectionReport <- function(sp){
  
  clgeo_report <- as.data.frame(do.call("rbind", lapply(1:length(sp), function(x){
    report <- unlist(clgeo_GeometryReport(sp[x,]))
  })), stringsAsFactors = FALSE)
  clgeo_report$valid <- as(clgeo_report$valid, "logical")
  
  return(clgeo_report)
}
