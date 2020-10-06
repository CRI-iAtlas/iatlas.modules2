
server <- function(input, output) {

  # # example data ----

  starwars_data <- shiny::reactive(example_starwars_data())
  starwars_group_data <- shiny::reactive(example_starwars_group_data())

  iris_data <- shiny::reactive(example_iris_data())
  iris_group_data <- shiny::reactive(example_iris_group_data())
  iris_feature_data <- shiny::reactive(examplre_iris_feature_data())

  # examples ----

  barplot_server(
    "barplot1",
    starwars_data,
    group_data = starwars_group_data,
    barplot_xlab = shiny::reactive("Species"),
    barplot_ylab = shiny::reactive("Height")
  )

  barplot_server(
    "barplot2",
    iris_data,
    group_data = iris_group_data,
    feature_data = iris_feature_data,
    barplot_xlab = shiny::reactive("Species"),
    drilldown = shiny::reactive(T)
  )
}
