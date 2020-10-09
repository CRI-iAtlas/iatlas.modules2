
server <- function(input, output) {

  # examples ----

  barplot_server(
    "barplot1",
    shiny::reactive(example_starwars_data_func),
    barplot_xlab = shiny::reactive("Species"),
    barplot_ylab = shiny::reactive("Height")
  )

  barplot_server(
    "barplot2",
    shiny::reactive(example_iris_data_func),
    feature_classes = shiny::reactive(c("Length", "Width")),
    barplot_xlab = shiny::reactive("Species"),
    drilldown = shiny::reactive(T)
  )

  barplot_server(
    "barplot3",
    shiny::reactive(example_iris_data_func),
    barplot_xlab = shiny::reactive("Species"),
    drilldown = shiny::reactive(T)
  )
}
