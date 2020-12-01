plotly_box <- function(
  data,
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
  format_func = format_plotly
  ) {

  data  %>%
    dplyr::select(
      "x"     = x_col,
      "y"     = y_col,
      "key"   = key_col,
      "text"  = text_col,
      "color" = color_col,
      "split" = split_col
    ) %>%
    plotly::plot_ly(
      x     = ~x,
      y     = ~y,
      split = ~split,
      color = ~color,
      key   = ~key,
      text  = ~text,
      source = source_name,
      colors = fill_colors,
      type = "box",
      boxpoints = "all",
      jitter = 0.7,
      pointpos = 0
    ) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab)
    ) %>%
    format_func()
}
