plotR <- function(plot_fun, ...) {
  plot_env <- parent.frame()

  plot_code <- substitute(plot_fun)

  if (missing(...)) {
    # If no additional arguments
    if (is.symbol(plot_code)) {
      # If 'plot_fun' is a function name, create a call to 'plot_fun()'
      plot_code <- substitute(plot_fun())
    } else {
      # 'plot_fun' is an expression or code block
      # Do nothing; 'plot_code' is already the expression
    }
  } else {
    # If additional arguments are provided, build a call to 'plot_fun(...)'
    plot_code <- substitute(plot_fun(...))
  }

  plot_obj <- list(
    plot_code = plot_code,
    env = plot_env
  )

  class(plot_obj) <- "plotR"

  return(plot_obj)
}

legendR <- function(input, ...) {
  if (is.function(input) || is.language(input)) {
    # If input is an expression or function
    plot_code <- input
    args <- list(...)
  } else {
    # Assume input is a plotting function
    plot_call <- match.call()
    plot_call[[1]] <- as.name(deparse(substitute(input)))
    plot_code <- plot_call
    args <- NULL
  }

  plot_obj <- list(
    plot_code = plot_code,
    args = args,
    env = parent.frame()
  )

  class(plot_obj) <- "legendR"
  return(plot_obj)
}
