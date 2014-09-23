# clgeo_SuspiciousFeatures.R
# --------------------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2014-09-23
#
clgeo_SuspiciousFeatures <- function(report, errors.only = NULL){
  
  features <- sapply(1:nrow(report), function(x){
    idx <- NA
    if(!as(report[x,]$valid, "logical")) idx <- x
    if(!is.null(errors.only)){
      idx <- NA
      error <- as(report[x,]$issue_type, "character")
      if(error %in% errors.only) idx <- x
    }
    return(idx)
  })
  
  features <- features[!is.na(features)]
  if(length(features) == 0) features <- NA
  
  return(features)
}