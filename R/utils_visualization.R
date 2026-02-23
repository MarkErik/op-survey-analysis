# Visualization Helper Functions
# Helper functions for creating charts and visualizations
#
# @author Course Instructor
# @version 2.0.0

#' Create horizontal bar chart
#'
#' Creates a horizontal bar chart with proper styling
#'
#' @param data Data frame with x and y columns
#' @param x_col Column name for x-axis (categories)
#' @param y_col Column name for y-axis (values)
#' @param fill_col Optional column for fill color
#' @param title Chart title
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @return ggplot2 object
#' @export
create_hbar_chart <- function(data, x_col, y_col, fill_col = NULL, title = "", x_label = "", y_label = "") {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = reorder(.data[[x_col]], -.data[[y_col]]), y = .data[[y_col]]))

  if (!is.null(fill_col)) {
    p <- p + ggplot2::aes(fill = .data[[fill_col]])
  }

  p <- p +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = title, x = x_label, y = y_label) +
    get_viz_theme()

  return(p)
}

#' Create Likert chart
#'
#' Creates a diverging bar chart for Likert scale data
#'
#' @param data Data frame with Likert responses
#' @param question_col Column name for question
#' @param title Chart title
#' @return ggplot2 object
#' @export
create_likert_chart <- function(data, question_col, title = "") {
  values <- data[[question_col]]
  values <- values[!is.na(values)]

  if (length(values) == 0) {
    return(NULL)
  }

  # Create distribution data
  dist_data <- data.frame(
    value = factor(1:5),
    count = sapply(1:5, function(x) sum(values == x))
  )

  colors <- c(
    "1" = "#d73027",
    "2" = "#fc8d59",
    "3" = "#fee08b",
    "4" = "#d9ef8b",
    "5" = "#1a9850"
  )

  labels <- c(
    "1" = "Strongly Disagree",
    "2" = "Disagree",
    "3" = "Neutral",
    "4" = "Agree",
    "5" = "Strongly Agree"
  )

  ggplot2::ggplot(dist_data, ggplot2::aes(x = value, y = count, fill = value)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_discrete(labels = labels, breaks = 1:5) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(title = title, x = "Response", y = "Count") +
    get_viz_theme() +
    ggplot2::theme(legend.position = "none")
}

#' Create section comparison chart
#'
#' Creates a chart comparing responses across sections
#'
#' @param data Survey data frame
#' @param question_col Question column name
#' @param section_col Section column name
#' @param title Chart title
#' @return ggplot2 object
#' @export
create_section_comparison_chart <- function(data, question_col, section_col = "What section are you in?", title = "") {
  if (!section_col %in% colnames(data)) {
    return(NULL)
  }

  section_means <- data %>%
    dplyr::filter(!is.na(.data[[section_col]]) & .data[[section_col]] != "") %>%
    dplyr::group_by(.data[[section_col]]) %>%
    dplyr::summarize(
      mean = mean(.data[[question_col]], na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

  if (nrow(section_means) == 0) {
    return(NULL)
  }

  colors <- RColorBrewer::brewer.pal(nrow(section_means), "Set2")

  ggplot2::ggplot(section_means, ggplot2::aes(
    x = reorder(.data[[section_col]], -mean),
    y = mean,
    fill = .data[[section_col]]
  )) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", mean)), vjust = -0.3) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(title = title, x = "Section", y = "Mean Score") +
    get_viz_theme() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylim(0, 5.5)
}

#' Create correlation heatmap
#'
#' Creates a heatmap visualization of correlation matrix
#'
#' @param cor_matrix Correlation matrix
#' @param labels Optional labels for variables
#' @param title Chart title
#' @return ggplot2 object
#' @export
create_correlation_heatmap <- function(cor_matrix, labels = NULL, title = "Correlation Matrix") {
  # Convert to long format
  cor_df <- reshape2::melt(cor_matrix)
  colnames(cor_df) <- c("Var1", "Var2", "value")

  # Apply labels if provided
  if (!is.null(labels)) {
    cor_df$Var1 <- labels[match(cor_df$Var1, rownames(cor_matrix))]
    cor_df$Var2 <- labels[match(cor_df$Var2, colnames(cor_matrix))]
  }

  ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "#d73027",
      mid = "#ffffff",
      high = "#1a9850",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text = ggplot2::element_text(size = 8)
    ) +
    ggplot2::labs(title = title, x = NULL, y = NULL, fill = "Correlation")
}

#' Create box plot
#'
#' Creates a box plot for distribution visualization
#'
#' @param data Data frame
#' @param y_col Column for y-axis
#' @param x_col Optional column for x-axis (grouping)
#' @param title Chart title
#' @param y_label Y-axis label
#' @return ggplot2 object
#' @export
create_box_plot <- function(data, y_col, x_col = NULL, title = "", y_label = "Value") {
  p <- ggplot2::ggplot(data, ggplot2::aes(y = .data[[y_col]]))

  if (!is.null(x_col)) {
    p <- p + ggplot2::aes(x = .data[[x_col]], fill = .data[[x_col]])
  }

  p <- p +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = title, y = y_label, x = NULL) +
    get_viz_theme()

  if (!is.null(x_col)) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}

#' Create distribution histogram
#'
#' Creates a histogram with density overlay
#'
#' @param data Data frame
#' @param x_col Column to plot
#' @param bins Number of bins
#' @param title Chart title
#' @param x_label X-axis label
#' @return ggplot2 object
#' @export
create_histogram <- function(data, x_col, bins = 30, title = "", x_label = "Value") {
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_col]])) +
    ggplot2::geom_histogram(bins = bins, fill = "#3498db", alpha = 0.7) +
    ggplot2::geom_density(ggplot2::aes(y = ..count..), color = "#2c3e50", size = 1) +
    ggplot2::labs(title = title, x = x_label, y = "Count") +
    get_viz_theme()
}

#' Create radar chart
#'
#' Creates a radar/spider chart for multi-variable comparison
#'
#' @param data Data frame with variables in rows
#' @param value_col Column containing values
#' @param label_col Column containing labels
#' @param title Chart title
#' @return ggplot2 object
#' @export
create_radar_chart <- function(data, value_col, label_col, title = "") {
  # Prepare data
  plot_data <- data.frame(
    variable = data[[label_col]],
    value = data[[value_col]]
  )

  # Complete the circle
  plot_data <- rbind(plot_data, plot_data[1, ])

  # Calculate angles
  n_vars <- nrow(plot_data) - 1
  angles <- seq(0, 2 * pi, length.out = n_vars + 1)[-1]

  plot_data$angle <- angles

  ggplot2::ggplot(plot_data, ggplot2::aes(x = angle, y = value, group = 1)) +
    ggplot2::geom_polygon(fill = "#3498db", alpha = 0.3) +
    ggplot2::geom_line(color = "#3498db", size = 2) +
    ggplot2::coord_polar() +
    ggplot2::scale_x_continuous(
      breaks = angles,
      labels = plot_data$variable[-nrow(plot_data)]
    ) +
    ggplot2::labs(title = title) +
    get_viz_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 8),
      axis.text.y = ggplot2::element_blank()
    )
}

#' Create stacked bar chart
#'
#' Creates a stacked bar chart for proportions
#'
#' @param data Data frame
#' @param x_col X-axis column
#' @param fill_col Fill category column
#' @param title Chart title
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @return ggplot2 object
#' @export
create_stacked_bar <- function(data, x_col, fill_col, title = "", x_label = "", y_label = "Count") {
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_col]], fill = .data[[fill_col]])) +
    ggplot2::geom_bar(position = "fill") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(title = title, x = x_label, y = y_label, fill = fill_col) +
    get_viz_theme() +
    ggplot2::theme(legend.position = "bottom")
}

#' Create violin plot
#'
#' Creates a violin plot for distribution comparison
#'
#' @param data Data frame
#' @param y_col Y-axis column
#' @param x_col X-axis column (grouping)
#' @param title Chart title
#' @param y_label Y-axis label
#' @return ggplot2 object
#' @export
create_violin_plot <- function(data, y_col, x_col = NULL, title = "", y_label = "Value") {
  p <- ggplot2::ggplot(data, ggplot2::aes(y = .data[[y_col]]))

  if (!is.null(x_col)) {
    p <- p + ggplot2::aes(x = .data[[x_col]], fill = .data[[x_col]])
  }

  p <- p +
    ggplot2::geom_violin() +
    ggplot2::geom_boxplot(width = 0.1, fill = "white") +
    ggplot2::labs(title = title, y = y_label, x = NULL) +
    get_viz_theme()

  if (!is.null(x_col)) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}

#' Create scatter plot with regression line
#'
#' Creates a scatter plot with optional regression line
#'
#' @param data Data frame
#' @param x_col X-axis column
#' @param y_col Y-axis column
#' @param show_line Whether to show regression line
#' @param title Chart title
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @return ggplot2 object
#' @export
create_scatter_plot <- function(data, x_col, y_col, show_line = TRUE, title = "", x_label = "", y_label = "") {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::labs(title = title, x = x_label, y = y_label) +
    get_viz_theme()

  if (show_line) {
    p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#3498db")
  }

  return(p)
}

#' Create heatmap with annotations
#'
#' Creates a heatmap with value annotations
#'
#' @param data Matrix or data frame for heatmap
#' @param low Color for low values
#' @param mid Color for mid values
#' @param high Color for high values
#' @param title Chart title
#' @return ggplot2 object
#' @export
create_annotated_heatmap <- function(data, low = "#d73027", mid = "#ffffff", high = "#1a9850", title = "") {
  # Convert to long format
  if (is.matrix(data)) {
    heatmap_data <- reshape2::melt(data)
    colnames(heatmap_data) <- c("x", "y", "value")
  } else {
    heatmap_data <- data
  }

  ggplot2::ggplot(heatmap_data, ggplot2::aes(x = x, y = y, fill = value, label = sprintf("%.2f", value))) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(color = "black", size = 3) +
    ggplot2::scale_fill_gradient2(low = low, mid = mid, high = high, midpoint = 0) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text = ggplot2::element_text(size = 8)
    ) +
    ggplot2::labs(title = title, x = NULL, y = NULL, fill = "Value")
}

#' Get color palette
#'
#' Returns a color palette for visualizations
#'
#' @param n Number of colors needed
#' @param palette Palette name
#' @return Vector of colors
#' @export
get_color_palette <- function(n, palette = "Set2") {
  if (n <= 8) {
    RColorBrewer::brewer.pal(n, palette)
  } else {
    grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, palette))(n)
  }
}

#' Format chart for export
#'
#' Prepares a chart for export with proper sizing
#'
#' @param plot ggplot2 object
#' @param width Width in inches
#' @param height Height in inches
#' @param dpi Resolution
#' @export
export_chart <- function(plot, width = 10, height = 8, dpi = 300) {
  ggplot2::ggsave(
    plot = plot,
    filename = "chart_export.png",
    width = width,
    height = height,
    dpi = dpi,
    units = "in"
  )
}
