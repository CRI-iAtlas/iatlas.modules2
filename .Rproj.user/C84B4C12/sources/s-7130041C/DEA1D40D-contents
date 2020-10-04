
server <- function(input, output) {

  # example data ----
  starwars_data <- shiny::reactive({
    dplyr::select(
      dplyr::starwars,
      "sample" = "name",
      "x" = "species",
      "color" = "gender",
      "y" = "height"
    )
  })

  starwars_group_data <- shiny::reactive({
    starwars_data() %>%
      dplyr::select("group" = "x") %>%
      dplyr::distinct() %>%
      dplyr::mutate("description" = stringr::str_c("Species: ", .data$group))
  })

  iris_data <- shiny::reactive({
    iris %>%
      dplyr::as_tibble() %>%
      dplyr::mutate("sample" = as.character(1:dplyr::n())) %>%
      tidyr::pivot_longer(!c("Species", "sample"), names_to = "color", values_to = "y") %>%
      dplyr::rename("x" = "Species")
  })

  iris_group_data <- shiny::reactive({
    iris_data() %>%
      dplyr::select("group" = "x") %>%
      dplyr::distinct() %>%
      dplyr::mutate("description" = stringr::str_c("Species: ", .data$group))
  })

  iris_feature_data <- shiny::reactive({
    dplyr::tribble(
      ~class,   ~feature,
      "Length", "Sepal.Length",
      "Width",  "Sepal.Width",
      "Length", "Petal.Length",
      "Width",  "Petal.Width"
    )
  })

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
