plotly_violin <- function(
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
  points = NULL,
  format_func = format_plotly,
  showlegend = T
  ) {

  select_cols <- c(
    "x"     = x_col,
    "y"     = y_col,
    "key"   = key_col,
    "text"  = text_col,
    "color" = color_col,
    "split" = split_col
  )

  data <- dplyr::select(data, select_cols)

  plotly::plot_ly(
    data,
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
    box = list(
      visible = TRUE
    ),
    meanline = list(
      visible = TRUE
    )) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab)
    ) %>%
    format_plotly() %>%
    I
}
