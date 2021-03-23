#' Plotly Scatter
#'
#' @param plot_data A dataframe
#' @param x_col A string
#' @param y_col A string
#' @param color_col A string or NA
#' @param key_col A string or NA
#' @param text_col A string or NA
#' @param xlab A string
#' @param ylab A string
#' @param title A string
#' @param source_name A string or NULL
#' @param identity_line True or False
#' @param horizontal_line True or False
#' @param horizontal_line_y A numeric or NULL
#' @param format_func A function
#'
#' @export
#' @importFrom magrittr %>%
plotly_scatter <- function(
  plot_data,
  x_col = "x",
  y_col = "y",
  color_col = x_col,
  key_col = x_col,
  text_col = x_col,
  xlab = "",
  ylab = "",
  title = "",
  source_name = NULL,
  identity_line = FALSE,
  horizontal_line = FALSE,
  horizontal_line_y = NULL,
  format_func = format_plotly
) {

  p <- plot_data %>%
    dplyr::select(dplyr::all_of(c(
      "x"     = x_col,
      "y"     = y_col,
      "key"   = key_col,
      "text"  = text_col
    ))) %>%
    plotly::plot_ly(
      x = ~x,
      y = ~y,
      text = ~text,
      key = ~key,
      source = source_name
    ) %>%
    plotly::add_markers(
      alpha = 0.5,
      hoverinfo = 'text',
      text = ~text,
      textposition = 'top left'
    ) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab)
    )

  # if (horizontal_line) {
  #   p <- p %>%
  #     plotly::layout(
  #       shapes = list(
  #         type = "line",
  #         x0 = min(data$x),
  #         y0 = horizontal_line_y,
  #         x1 = max(data$x),
  #         y1 = horizontal_line_y,
  #         line = list(color = "black", dash = "dot", alpha = 0.5)
  #       ))
  # }

  if (identity_line) {
    p <- p %>%
      plotly::layout(
        shapes = list(
          type = "line",
          x0 = min(plot_data$x),
          y0 = min(plot_data$y),
          x1 = max(plot_data$x),
          y1 = max(plot_data$y),
          line = list(color = "black", dash = "dot", alpha = 0.5)
        ),
        xaxis = list(range(0, 1)),
        yaxis = list(range(0, 1))
      )
  }

  if(!is.null(format_func)){
    p <- format_func(p)
  }

  return(p)
}
