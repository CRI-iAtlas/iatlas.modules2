
server <- function(input, output) {

  # # example data ----

  starwars_data <- shiny::reactive(example_starwars_data())
  iris_data <- shiny::reactive(example_iris_data())

  # examples ----

  barplot_server(
    "barplot1",
    starwars_data,
    barplot_xlab = shiny::reactive("Species"),
    barplot_ylab = shiny::reactive("Height")
  )

  barplot_server(
    "barplot2",
    iris_data,
    barplot_xlab = shiny::reactive("Species"),
    drilldown = shiny::reactive(T),
    x_default = shiny::reactive("Petal.Length")
  )
}
