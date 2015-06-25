#===================================================================================================
#' Get max depth of a recursive list
#' 
#' \url{http://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r}
#' 
#' @param this (\code{list})
depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)
