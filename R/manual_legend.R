place_legend_interactive <- function(legend_text, plot_expr, margin = 0.05, ...) {
  # Get the RStudio graphics device number
  rstudio_dev <- dev.cur()

  # Detect OS and open the appropriate graphics device
  os <- Sys.info()[["sysname"]]
  if (os == "Windows") {
    windows()
  } else if (os == "Darwin") {
    quartz()
  } else {
    x11()
  }

  # Get the new device number
  external_dev <- dev.cur()

  # Evaluate the plotting expression on the new device
  eval(plot_expr)

  # Step 1: Get plot dimensions on external device
  usr <- par("usr")
  xlim <- usr[1:2]
  ylim <- usr[3:4]

  # Step 2: Normalize the plot dimensions
  normalize <- function(value, min_val, max_val) {
    (value - min_val) / (max_val - min_val)
  }

  # Step 3: Let the user select a position
  cat("Click a spot to place the legend...\n")
  user_point <- locator(1)
  if (is.null(user_point)) {
    cat("No point selected. Aborting legend placement.\n")
    dev.off(external_dev)  # Close the external device
    return()
  }
  user_x_norm <- normalize(user_point$x, xlim[1], xlim[2])
  user_y_norm <- normalize(user_point$y, ylim[1], ylim[2])

  # Step 4: Snap to nearest edge if within the margin
  if (user_y_norm <= margin) {
    user_y_norm <- 0  # Snap to bottom
  } else if (user_y_norm >= 1 - margin) {
    user_y_norm <- 1  # Snap to top
  }
  if (user_x_norm <= margin) {
    user_x_norm <- 0  # Snap to left
  } else if (user_x_norm >= 1 - margin) {
    user_x_norm <- 1  # Snap to right
  }

  # Close the external device
  dev.off(external_dev)

  # Switch back to the RStudio graphics device
  dev.set(rstudio_dev)

  # Re-evaluate the plotting expression on the RStudio device
  eval(plot_expr)

  # Get plot dimensions on RStudio device
  usr <- par("usr")
  xlim <- usr[1:2]
  ylim <- usr[3:4]

  # Denormalize the user-selected position using the new xlim and ylim
  denormalize <- function(value, min_val, max_val) {
    value * (max_val - min_val) + min_val
  }
  adjusted_x <- denormalize(user_x_norm, xlim[1], xlim[2])
  adjusted_y <- denormalize(user_y_norm, ylim[1], ylim[2])

  # Recompute legend dimensions on RStudio device
  legend_info <- legend(
    x = xlim[1], y = ylim[2],
    legend = legend_text, plot = FALSE, ...
  )
  legend_width <- legend_info$rect$w
  legend_height <- legend_info$rect$h

  # Ensure the legend fits within the plot bounds
  adjusted_x <- max(xlim[1] + legend_width / 2, min(adjusted_x, xlim[2] - legend_width / 2))
  adjusted_y <- max(ylim[1] + legend_height / 2, min(adjusted_y, ylim[2] - legend_height / 2))

  # Step 7: Add the legend at the adjusted position on the RStudio device
  legend(
    x = adjusted_x, y = adjusted_y,
    legend = legend_text, xjust = 0.5, yjust = 0.5, ...
  )

  # Output information about the placement
  cat("Selected position (normalized):", user_x_norm, user_y_norm, "\n")
  cat("Adjusted position (original):", adjusted_x, adjusted_y, "\n")
}
