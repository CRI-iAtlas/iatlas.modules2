#' Plotly Box
#'
#' @param plot_data A dataframe
#' @param x_col A string
#' @param y_col A string
#' @param color_col A string or NA
#' @param key_col A string or NA
#' @param text_col A string or NA
#' @param split_col A string or NA
#' @param xlab A string
#' @param ylab A string
#' @param title A string
#' @param source_name A string or NULL
#' @param fill_colors A string or NULL
#' @param points A string or NULL
#' @param showlegend True or False
#' @param format_func A function
#'
#' @importFrom magrittr %>%
#'
#' @export
plotly_violin <- function(
  plot_data,
  x_col = "x",
  y_col = "y",
  color_col = x_col,
  key_col = x_col,
  text_col = x_col,
  split_col = x_col,
  xlab = "",
  ylab = "",
  title = "",
  source_name = NULL,
  fill_colors = NULL,
  points = NULL,
  showlegend = T,
  format_func = format_plotly
  ) {

  plot_data %>%
    dplyr::select(dplyr::all_of(c(
      "x"     = x_col,
      "y"     = y_col,
      "key"   = key_col,
      "text"  = text_col,
      "color" = color_col,
      "split" = split_col
    ))) %>%
    plotly::plot_ly(
      x = ~x,
      y = ~y,
      split = ~split,
      color = ~color,
      key = ~key,
      text = ~text,
      points = points,
      source = source_name,
      colors = fill_colors,
      type = 'violin',
      hoverinfo = 'text',
      showlegend = showlegend,
      box = list(visible = TRUE),
      meanline = list(visible = TRUE)
    ) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab)
    ) %>%
    format_plotly()
}
