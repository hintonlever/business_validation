rsplit <- function(dd) {
  col <- names(dd)[1]
  print(col)
  dat <- unique(dd[[1]])
  xx <- lapply(unique(dat), function(x) {
    z <- setNames(list(x), col)
    if(ncol(dd)>1) {
      z[[names(dd)[2]]] <- rsplit(dd[dat==x,-1, drop=FALSE])
    }
    z
  })
  return(xx)
}
