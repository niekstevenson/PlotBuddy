# Helper function to create a key mapping plot indices to layout positions
make_key <- function(layout_mat, plot_order = NULL) {
  n_plots <- max(layout_mat)
  if (is.null(plot_order)) {
    plot_order <- 1:n_plots
  }
  key <- setNames(plot_order, as.character(1:n_plots))
  return(key)
}

# Helper function to modify the layout matrix and adjust widths and heights
change_layout <- function(layout_mat, widths, heights, key, side = "right", size = 0.1, content = "empty") {
  # Validate 'side' argument
  if (!side %in% c("top", "bottom", "left", "right")) {
    stop("Invalid 'side' argument. Choose from 'top', 'bottom', 'left', or 'right'.")
  }

  max_key <- max(as.numeric(names(key)))
  new_panel_num <- max(layout_mat) + 1

  if (side %in% c("left", "right")) {
    # Add a column
    n_rows <- nrow(layout_mat)
    new_col <- matrix(new_panel_num, nrow = n_rows, ncol = 1)
    if (side == "left") {
      layout_mat <- cbind(new_col, layout_mat)
      widths <- c(size, widths * (1 - size))
    } else {
      layout_mat <- cbind(layout_mat, new_col)
      widths <- c(widths * (1 - size), size)
    }
  } else {
    # Add a row
    n_cols <- ncol(layout_mat)
    new_row <- matrix(new_panel_num, nrow = 1, ncol = n_cols)
    if (side == "top") {
      layout_mat <- rbind(new_row, layout_mat)
      heights <- c(size, heights * (1 - size))
    } else {
      layout_mat <- rbind(layout_mat, new_row)
      heights <- c(heights * (1 - size), size)
    }
  }

  # Update key
  key <- c(key, setNames(content, as.character(new_panel_num)))

  return(list(layout_mat = layout_mat, widths = widths, heights = heights, key = key))
}

# Helper function to plot the data using the layout and key
plot_with_layout <- function(data_list, layout_mat, widths, heights, key,
                             common_title = NULL, common_xlab = NULL, common_ylab = NULL) {
  # Save current graphical parameters
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Apply the layout
  layout(layout_mat, widths = widths, heights = heights)

  # Set margins for plots
  par(mar = c(2, 2, 2, 2))

  # Plotting loop
  for (i in as.numeric(names(key))) {
    content <- key[as.character(i)]
    if (content %in% c("title", "xlab", "ylab", "legend", "empty")) {
      # Handle special panels
      par(mar = c(0, 0, 0, 0))
      plot.new()
      if (content == "title" && !is.null(common_title)) {
        text(0.5, 0.5, common_title, cex = 1.5)
      } else if (content == "xlab" && !is.null(common_xlab)) {
        text(0.5, 0.5, common_xlab, cex = 1.2)
      } else if (content == "ylab" && !is.null(common_ylab)) {
        text(0.5, 0.5, common_ylab, cex = 1.2, srt = 90)
      } else if (content == "legend") {
        legend("center", legend = "input", pch = 1, bty = "n")
      }
    } else {
      # Plot the data
      plot_index <- as.numeric(content)
      plot(data_list[[plot_index]], main = "", xlab = "", ylab = "")
    }
  }

  # Restore graphical parameters
  par(old_par)
}

#' Arrange Multiple Plots with Common Labels and Legend
#'
#' This function arranges multiple plots in a specified layout, adding common axis labels, a common title, and a legend at specified positions. The plots can be scaled according to specified widths and heights.
#'
#' @param data_list A list of data vectors or plot objects to be displayed.
#' @param layout_mat A numeric matrix specifying the initial layout of the plots. Each element represents a plot number. If \code{NULL}, an appropriate layout matrix is generated based on the length of \code{data_list}.
#' @param legend_pos A character string specifying the position of the legend. Options are \code{"top"}, \code{"bottom"}, \code{"left"}, or \code{"right"}. Default is \code{"right"}.
#' @param legend_size A numeric value between 0 and 1 specifying the proportion of space allocated to the legend relative to the whole plotting area. Default is \code{0.2}.
#' @param widths A numeric vector specifying the relative widths of the plot columns. If \code{NULL}, columns are equally sized.
#' @param heights A numeric vector specifying the relative heights of the plot rows. If \code{NULL}, rows are equally sized.
#' @param common_title A character string for the common title of the plots. If \code{NULL}, no common title is added.
#' @param common_xlab A character string for the common x-axis label. If \code{NULL}, no common x-axis label is added.
#' @param common_ylab A character string for the common y-axis label. If \code{NULL}, no common y-axis label is added.
#' @param label_size A numeric value between 0 and 1 specifying the proportion of space allocated to each common label relative to the whole plotting area. Default is \code{0.05}.
#'
#' @details
#' The \code{plot_arrange} function allows users to arrange multiple plots in a grid layout with common axis labels, a common title, and a legend. The layout can be customized by specifying the position and size of the legend and labels, as well as the widths and heights of the plot columns and rows.
#'
#' If no \code{layout_mat} is provided, the function generates a layout matrix based on the number of plots in \code{data_list}. The \code{widths} and \code{heights} parameters allow for custom scaling of plot columns and rows, respectively.
#'
#' The function internally uses helper functions to manage the layout and plotting:
#' \itemize{
#'   \item \code{make_key}: Creates a key mapping plot indices to positions in the layout matrix.
#'   \item \code{change_layout}: Modifies the layout matrix to add new rows or columns for labels and legend, adjusting widths and heights accordingly.
#'   \item \code{plot_with_layout}: Uses the updated layout, widths, heights, and key to generate the final plot, handling the plotting of data, labels, and legend.
#' }
#'
#' @examples
#' # Example 1: Basic usage with default settings
#' data_list <- list(rnorm(100), rnorm(100), rnorm(100), rnorm(100))
#' plot_arrange(
#'   data_list = data_list,
#'   common_title = "Common Title",
#'   common_xlab = "Common X-Axis Label",
#'   common_ylab = "Common Y-Axis Label"
#' )
#'
#' # Example 2: Custom layout and legend position
#' custom_layout <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
#' plot_arrange(
#'   data_list = data_list,
#'   layout_mat = custom_layout,
#'   legend_pos = "bottom",
#'   legend_size = 0.15,
#'   common_title = "Custom Plot",
#'   common_xlab = "Time",
#'   common_ylab = "Value"
#' )
#'
#' # Example 3: Custom widths and heights
#' plot_arrange(
#'   data_list = data_list,
#'   widths = c(0.6, 0.4),
#'   heights = c(0.7, 0.3),
#'   legend_pos = "left",
#'   legend_size = 0.2,
#'   common_title = "Custom Sized Plots",
#'   common_xlab = "X-Axis",
#'   common_ylab = "Y-Axis"
#' )
#'
#' @export
plot_arrange <- function(data_list, layout_mat = NULL, legend_pos = "right", legend_size = 0.2,
                         widths = NULL, heights = NULL, common_title = NULL,
                         common_xlab = NULL, common_ylab = NULL, label_size = 0.05) {
  # Step 1: Generate layout matrix if not provided
  n_plots <- length(data_list)
  if (is.null(layout_mat)) {
    n_col <- ceiling(sqrt(n_plots))
    n_row <- ceiling(n_plots / n_col)
    layout_mat <- matrix(1:n_plots, nrow = n_row, ncol = n_col, byrow = TRUE)
  }

  # Step 2: Initialize widths and heights if not provided
  if (is.null(widths)) {
    widths <- rep(1 / ncol(layout_mat), ncol(layout_mat))
  }
  if (is.null(heights)) {
    heights <- rep(1 / nrow(layout_mat), nrow(layout_mat))
  }

  # Step 3: Generate the initial key
  key <- make_key(layout_mat)

  # Step 4: Add labels and legend using change_layout
  # Adjust label_size and legend_size if necessary
  label_size <- label_size
  legend_size <- legend_size

  # Add common x-axis label
  if (!is.null(common_xlab)) {
    result <- change_layout(layout_mat = layout_mat,
                            widths = widths, heights = heights,
                            key = key, side = "bottom",
                            size = label_size, content = "xlab")
    layout_mat <- result$layout_mat
    widths <- result$widths
    heights <- result$heights
    key <- result$key
  }

  # Add common title
  if (!is.null(common_title)) {
    result <- change_layout(layout_mat = layout_mat, widths = widths,
                            heights = heights, key = key, side = "top", size = label_size,
                            content = "title")
    layout_mat <- result$layout_mat
    widths <- result$widths
    heights <- result$heights
    key <- result$key
  }

  # Add common y-axis label
  if (!is.null(common_ylab)) {
    result <- change_layout(layout_mat = layout_mat,
                            widths = widths, heights = heights,
                            key = key, side = "left", size = label_size, content = "ylab")
    layout_mat <- result$layout_mat
    widths <- result$widths
    heights <- result$heights
    key <- result$key
  }

  # Add legend
  if (!is.null(legend_pos)) {
    result <- change_layout(layout_mat = layout_mat,
                            widths = widths, heights = heights, key = key,
                            side = legend_pos, size = legend_size, content = "legend")
    layout_mat <- result$layout_mat
    widths <- result$widths
    heights <- result$heights
    key <- result$key
  }

  # Step 5: Plot the data using plot_with_layout
  plot_with_layout(
    data_list = data_list, layout_mat = layout_mat, widths = widths,
    heights = heights, key = key, common_title = common_title,
    common_xlab = common_xlab, common_ylab = common_ylab
  )
}
