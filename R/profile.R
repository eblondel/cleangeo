.onLoad <- function (libname, pkgname) {
  
  #make sure sf relies on... "sf" (aka ISO/OGC Simple Features)! and not s2
  options(sf_use_s2 = FALSE)
  
}