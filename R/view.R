#'View QW objects
#'
#'View QW objects, specifically a fix for RStudio.
#'
#'@param x dataframe to view.
#'@keywords custom view
#'@export
View <- function(x) {
  xn <- deparse(substitute(x))
  vf <- get("View", pos=3, mode="function")
  invisible(vf(format(x), title=xn))
}