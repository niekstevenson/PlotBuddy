plot_with_legend <- function(data_list, layout_mat = NULL, position = "right", legend_mar = 0.2,
                             widths = NULL, heights = NULL, common_title = NULL,
                             common_xlab = NULL, common_ylab = NULL, label_mar = 0.05) {
  # Validate 'position' argument
  if (!position %in% c("top", "bottom", "left", "right")) {
    stop("Invalid position argument. Choose from 'top', 'bottom', 'left', or 'right'.")
  }

  # Validate 'legend_mar' and 'label_mar' arguments
  if (!is.numeric(legend_mar) || legend_mar < 0 || legend_mar >= 1) {
    stop("legend_mar must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(label_mar) || label_mar < 0 || label_mar >= 1) {
    stop("label_mar must be a numeric value between 0 and 1.")
  }

  # Save current graphical parameters
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Number of plots
  n_plots <- length(data_list)

  # Determine layout if not specified
  if (is.null(layout_mat)) {
    n_col <- ceiling(sqrt(n_plots))
    n_row <- ceiling(n_plots / n_col)
    layout_mat <- matrix(1:n_plots, nrow = n_row, ncol = n_col, byrow = TRUE)
  }

  # Adjust the layout matrix to include labels first
  adjust_result <- adjust_layout(layout_mat, legend = FALSE,
                                 common_title = common_title,
                                 common_xlab = common_xlab,
                                 common_ylab = common_ylab)
  layout_mat <- adjust_result$layout_mat
  added_panels <- adjust_result$added_panels

  # Now, add the legend panel and recalculate widths or heights
  max_panel <- max(layout_mat)
  panel_num <- max_panel + 1
  added_panels$legend <- panel_num

  if (position %in% c("left", "right")) {
    # Add a column for the legend
    legend_col <- matrix(panel_num, nrow = nrow(layout_mat), ncol = 1)
    if (position == "left") {
      layout_mat <- cbind(legend_col, layout_mat)
    } else {
      layout_mat <- cbind(layout_mat, legend_col)
    }
  } else {
    # Add a row for the legend
    legend_row <- matrix(panel_num, nrow = 1, ncol = ncol(layout_mat))
    if (position == "top") {
      layout_mat <- rbind(legend_row, layout_mat)
    } else {
      layout_mat <- rbind(layout_mat, legend_row)
    }
  }

  # Calculate widths and heights
  n_rows <- nrow(layout_mat)
  n_cols <- ncol(layout_mat)

  widths <- rep(1, n_cols)
  heights <- rep(1, n_rows)

  total_width <- 1
  total_height <- 1

  # Adjust widths for legend and ylab
  ylab_index <- NULL
  legend_index <- NULL

  if (position %in% c("left", "right")) {
    if (position == "left") {
      legend_index <- 1
      widths[legend_index] <- legend_mar
      total_width <- total_width - legend_mar

      if (!is.null(common_ylab)) {
        ylab_index <- 2
        widths[ylab_index] <- label_mar
        total_width <- total_width - label_mar
      }
    } else {
      legend_index <- n_cols
      widths[legend_index] <- legend_mar
      total_width <- total_width - legend_mar

      if (!is.null(common_ylab)) {
        ylab_index <- n_cols - 1
        widths[ylab_index] <- label_mar
        total_width <- total_width - label_mar
      }
    }
  } else {
    if (!is.null(common_ylab)) {
      ylab_index <- 1
      widths[ylab_index] <- label_mar
      total_width <- total_width - label_mar
    }
  }

  # Normalize plot widths
  plot_cols <- setdiff(1:n_cols, c(legend_index, ylab_index))
  widths[plot_cols] <- widths[plot_cols] / sum(widths[plot_cols]) * total_width

  # Adjust heights for legend, title, and xlab
  title_index <- NULL
  xlab_index <- NULL

  if (position %in% c("top", "bottom")) {
    if (position == "top") {
      legend_index <- 1
      heights[legend_index] <- legend_mar
      total_height <- total_height - legend_mar

      if (!is.null(common_title)) {
        title_index <- 2
        heights[title_index] <- label_mar
        total_height <- total_height - label_mar
      }
    } else {
      legend_index <- n_rows
      heights[legend_index] <- legend_mar
      total_height <- total_height - legend_mar

      if (!is.null(common_xlab)) {
        xlab_index <- n_rows - 1
        heights[xlab_index] <- label_mar
        total_height <- total_height - label_mar
      }
    }
  } else {
    if (!is.null(common_title)) {
      title_index <- 1
      heights[title_index] <- label_mar
      total_height <- total_height - label_mar
    }
    if (!is.null(common_xlab)) {
      xlab_index <- n_rows
      heights[xlab_index] <- label_mar
      total_height <- total_height - label_mar
    }
  }

  # Normalize plot heights
  plot_rows <- setdiff(1:n_rows, c(legend_index, title_index, xlab_index))
  heights[plot_rows] <- heights[plot_rows] / sum(heights[plot_rows]) * total_height

  # Apply the layout
  layout(layout_mat, widths = widths, heights = heights)

  # Set margins for plots
  par(mar = c(3, 3, 2, 2))

  # Plotting loop
  max_panel <- max(layout_mat)
  plot_index <- 1
  for (i in 1:max_panel) {
    if (i %in% layout_mat) {
      if (i <= n_plots) {
        # Plot the data
        plot(data_list[[plot_index]], main = "", xlab = "", ylab = "")
        plot_index <- plot_index + 1
      } else if (i == added_panels$legend) {
        # Plot the legend
        par(mar = c(0, 0, 0, 0))
        plot.new()
        legend("center", legend = "input", pch = 1, bty = "n")
      } else if (!is.null(added_panels$title) && i == added_panels$title) {
        # Plot the common title
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, common_title, cex = 1.5)
      } else if (!is.null(added_panels$xlab) && i == added_panels$xlab) {
        # Plot the common x-axis label
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, common_xlab, cex = 1.2)
      } else if (!is.null(added_panels$ylab) && i == added_panels$ylab) {
        # Plot the common y-axis label
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, common_ylab, cex = 1.2, srt = 90)
      } else {
        # Empty plot
        plot.new()
      }
    }
  }

  # Restore graphical parameters
  par(old_par)
}

adjust_layout <- function(layout_mat, legend = TRUE, legend_position = "right",
                          common_title = NULL, common_xlab = NULL, common_ylab = NULL) {
  # Initialize variables
  n_rows <- nrow(layout_mat)
  n_cols <- ncol(layout_mat)
  max_panel <- max(layout_mat)
  panel_num <- max_panel + 1
  added_panels <- list()

  # Adjust for common y-axis label (add a column)
  if (!is.null(common_ylab)) {
    ylab_col <- matrix(panel_num, nrow = n_rows, ncol = 1)
    layout_mat <- cbind(ylab_col, layout_mat)
    added_panels$ylab <- panel_num
    panel_num <- panel_num + 1
  }

  # Adjust for common x-axis label (add a row at the bottom)
  if (!is.null(common_xlab)) {
    xlab_row <- matrix(panel_num, nrow = 1, ncol = ncol(layout_mat))
    layout_mat <- rbind(layout_mat, xlab_row)
    added_panels$xlab <- panel_num
    panel_num <- panel_num + 1
  }

  # Adjust for common title (add a row at the top)
  if (!is.null(common_title)) {
    title_row <- matrix(panel_num, nrow = 1, ncol = ncol(layout_mat))
    layout_mat <- rbind(title_row, layout_mat)
    added_panels$title <- panel_num
    panel_num <- panel_num + 1
  }

  return(list(layout_mat = layout_mat, added_panels = added_panels))
}
