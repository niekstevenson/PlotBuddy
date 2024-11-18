#' Plot with Optimal Legend Placement
#'
#' This function plots a given dataset and places a legend in the emptiest space
#' along the edges of the plot. The position is determined by iteratively
#' finding the largest circle centered along the edges that does not touch any
#' plot points.
#'
#' @param x A numeric vector of x-coordinates for the plot data.
#' @param y A numeric vector of y-coordinates for the plot data.
#' @param legend_text A character vector specifying the legend text.
#' @param ... Additional arguments passed to the \code{\link{plot}} and \code{\link{legend}} functions.
#'
#' @return This function generates a plot and places a legend optimally along the edges.
#' It also prints information about the legend's position (both normalized and original coordinates)
#' and the radius of the largest empty circle found.
#'
#' @details
#' The function determines the optimal legend position by:
#' 1. Normalizing the data coordinates to the [0, 1] range.
#' 2. Iteratively growing circles along the plot edges until the largest
#'    circle that does not overlap any data points is found.
#' 3. Adjusting the legend position to fit within the plot bounds, considering
#'    the legend's width and height.
#'
#' @examples
#' # Example usage:
#' set.seed(123)
#' x <- 1:1000
#' y <- x + rnorm(1000)
#' auto_legend(
#'   x, y,
#'   legend_text = "test",
#'   col = "black", pch = 1
#' )
#'
#' @export
auto_legend <- function(x, y, legend_text, ...) {
  # Step 1: Plot the data
  plot(x, y, main = "Optimal Legend Placement", ...)

  # Step 2: Get plot dimensions
  usr <- par("usr")  # Get plot dimensions: c(xmin, xmax, ymin, ymax)
  xlim <- usr[1:2]
  ylim <- usr[3:4]

  # Step 3: Normalize data points to a [0, 1] range
  x_normalized <- (x - xlim[1]) / (xlim[2] - xlim[1])
  y_normalized <- (y - ylim[1]) / (ylim[2] - ylim[1])

  # Step 4: Define candidate positions strictly along normalized edges
  N <- 100  # Granularity for candidate positions
  candidate_positions <- data.frame(x = numeric(0), y = numeric(0))

  # Bottom edge
  bottom_x <- seq(0, 1, length.out = N)
  bottom_y <- rep(0, N)
  candidate_positions <- rbind(candidate_positions, data.frame(x = bottom_x, y = bottom_y))

  # Top edge
  top_x <- seq(0, 1, length.out = N)
  top_y <- rep(1, N)
  candidate_positions <- rbind(candidate_positions, data.frame(x = top_x, y = top_y))

  # Left edge
  left_x <- rep(0, N)
  left_y <- seq(0, 1, length.out = N)
  candidate_positions <- rbind(candidate_positions, data.frame(x = left_x, y = left_y))

  # Right edge
  right_x <- rep(1, N)
  right_y <- seq(0, 1, length.out = N)
  candidate_positions <- rbind(candidate_positions, data.frame(x = right_x, y = right_y))

  # Step 5: Iteratively maximize circle size
  max_radius <- numeric(nrow(candidate_positions))  # Store the maximum radius for each position

  for (i in seq_len(nrow(candidate_positions))) {
    cx <- candidate_positions$x[i]
    cy <- candidate_positions$y[i]

    # Initialize radius and step size
    radius <- 0
    step <- 0.01  # Adjust step size for finer resolution

    # Increase radius until the circle touches any data point
    while (TRUE) {
      distances <- sqrt((x_normalized - cx)^2 + (y_normalized - cy)^2)
      if (any(distances <= radius)) {
        break
      }
      radius <- radius + step
    }

    # Store the maximum radius that doesn't overlap
    max_radius[i] <- radius - step
  }

  # Step 6: Find the position with the largest circle
  best_index <- which.max(max_radius)
  best_normalized_x <- candidate_positions$x[best_index]
  best_normalized_y <- candidate_positions$y[best_index]
  best_radius <- max_radius[best_index]

  # Step 7: Rescale the best position back to the original coordinates
  best_x <- best_normalized_x * (xlim[2] - xlim[1]) + xlim[1]
  best_y <- best_normalized_y * (ylim[2] - ylim[1]) + ylim[1]

  # Step 8: Adjust for legend dimensions
  legend_info <- legend(
    x = xlim[1], y = ylim[2],
    legend = legend_text, plot = FALSE, ...
  )
  legend_width <- legend_info$rect$w
  legend_height <- legend_info$rect$h

  # Ensure the legend fits within the plot boundaries
  best_x <- max(xlim[1] + legend_width / 2, min(best_x, xlim[2] - legend_width / 2))
  best_y <- max(ylim[1] + legend_height / 2, min(best_y, ylim[2] - legend_height / 2))

  # Step 9: Plot the legend at the adjusted position
  legend(
    x = best_x, y = best_y,
    legend = legend_text, xjust = 0.5, yjust = 0.5, ...
  )

  # Output information about the placement
  cat("Best legend position (normalized):", best_normalized_x, best_normalized_y, "\n")
  cat("Best legend position (original):", best_x, best_y, "\n")
  cat("Max circle radius (normalized):", best_radius, "\n")
}
