#' Plotly Histogram
#'
#' @param plot_data A dataframe
#' @param x_col A string
#' @param xlab A string
#' @param ylab A string
#' @param title A string
#' @param source_name A string or NULL
#' @param format_func A function
#'
#' @importFrom magrittr %>%
#'
#' @export
plotly_histogram <- function(
  plot_data,
  x_col = "x",
  xlab = "",
  ylab = "Count",
  title = "",
  source_name = NULL,
  format_func = format_plotly
) {

  plot_data %>%
    dplyr::select(dplyr::all_of(c("x" = x_col))) %>%
    plotly::plot_ly(x = ~x) %>%
    plotly::add_histogram(alpha = 0.8) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab)
    ) %>%
    format_plotly()
}
