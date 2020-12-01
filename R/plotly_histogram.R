plotly_histogram <- function(
  plot_data,
  x_col = "x",
  x_lab = "",
  y_lab = "Count",
  title = "",
  source_name = NULL,
  format_func = format_plotly
) {

  plot_data %>%
    dplyr::select("x" = x_col) %>%
    plotly::plot_ly(x = ~x) %>%
    plotly::add_histogram(alpha = 0.8) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = x_lab),
      yaxis = list(title = y_lab)
    ) %>%
    format_plotly()
}
