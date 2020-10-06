test_that("barplot_server_starwars", {

  shiny::testServer(
    barplot_server,
    args = list(
      "plot_data" = shiny::reactive(example_starwars_data()),
      "group_data" = shiny::reactive(example_starwars_group_data())
    ),
    {
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")
      expect_error(
        barplot_eventdata(),
        regexp = "Click on above barplot.",
        class = c("shiny.silent.error")
      )
      expect_named(
        summarized_barplot_data(),
        c("x", "color", "text", "MEAN", "SE")
      )
    }
  )
})

test_that("barplot_server_iris", {

  shiny::testServer(
    barplot_server,
    args = list(
      "plot_data" = shiny::reactive(example_iris_data()),
      "group_data" = shiny::reactive(example_iris_group_data()),
      "feature_data" = shiny::reactive(examplre_iris_feature_data()),
      "drilldown" = shiny::reactive(T)
    ),
    {
      expect_type(output$feature_class_selection_ui, "list")
      session$setInputs("feature_class_choice" = "Length")
      expect_equal(barplot_features(), c("Sepal.Length", "Petal.Length"))
      expect_type(barplot_data(), "list")
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")
      expect_error(
        barplot_eventdata(),
        regexp = "Click on above barplot.",
        class = c("shiny.silent.error")
      )
    }
  )
})
