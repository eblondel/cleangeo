# clgeo_GeometryReport.R
# ----------------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2014-09-23
#
clgeo_GeometryReport <- function(spgeom){
  
  clgeo_report <- list(type = NA, valid = FALSE, issue_type = NA,
                       error_msg = NA, warning_msg = NA)
  
  report <- tryCatch({
    isvalid <- gIsValid(spgeom)
    if(isvalid) clgeo_report$valid <- TRUE
    return(clgeo_report)
    
  },warning = function(w){
    clgeo_report$type <- "rgeos_validity"
    clgeo_report$valid <- FALSE
    if(regexpr("self", conditionMessage(w), "match.length",
               ignore.case = TRUE) > 1) clgeo_report$issue_type = "GEOM_VALIDITY"
    clgeo_report$warning_msg <- conditionMessage(w)
    return(clgeo_report)
    
  },error = function(e){
    clgeo_report$type <- "rgeos_error"
    clgeo_report$valid <- FALSE
    if(regexpr("orphaned hole", conditionMessage(e), "match.length",
               ignore.case = TRUE) > 1) clgeo_report$issue_type = "ORPHANED_HOLE"
    clgeo_report$error_msg = conditionMessage(e)
    return(clgeo_report)
  })
  
  return(report)
}