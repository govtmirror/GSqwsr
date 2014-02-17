View <- function(x) {
  xn <- deparse(substitute(x))
  vf <- get("View", pos=3, mode="function")
  invisible(vf(format(x), title=xn))
}