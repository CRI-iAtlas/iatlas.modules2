
server <- function(input, output, session) {

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

  barplot_server(
    "barplot4",
    shiny::reactive(example_iris_data_func),
    barplot_xlab = shiny::reactive("Species"),
    drilldown = shiny::reactive(T),
    x_feature_input = shiny::reactive("Petal.Length"),
    y_feature_input = shiny::reactive("Petal.Width")
  )

  distributions_plot_server(
    "distplot1",
    plot_data_function = shiny::reactive(example_iris_data_func),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Species")
  )

  distributions_plot_server(
    "distplot2",
    plot_data_function = shiny::reactive(example_iris_data_func),
    features = shiny::reactive(
      example_iris_data() %>%
        dplyr::select(
          "feature_class",
          "feature_name" = "feature",
          "feature_display"
        ) %>%
        dplyr::distinct()
    ),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Species")
  )

  distributions_plot_server(
    "distplot3",
    plot_data_function = shiny::reactive(example_iris_data_func),
    features = shiny::reactive(
      example_iris_data() %>%
        dplyr::select(
          "Class1" = "feature_class",
          "Class2" = "feature_class2",
          "feature_name" = "feature",
          "feature_display"
        ) %>%
        dplyr::distinct()
    ),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Species"),
    scale_method_default = shiny::reactive("Log2"),
    feature_default = shiny::reactive("Petal.Length")
  )
}
