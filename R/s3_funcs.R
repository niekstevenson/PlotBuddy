#' Evaluates plotR expressions
#'
#' @param x a plotR object created with `plotR()`
#' @param ... optional additional arguments
#' @export
plot.plotR <- function(x, ...) {
  plot_code <- x$plot_code
  plot_env <- x$env
  eval(plot_code, envir = plot_env)
}
