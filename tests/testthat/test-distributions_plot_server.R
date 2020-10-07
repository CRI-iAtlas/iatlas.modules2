test_that("distributions_plot_server_iris", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "plot_data" = shiny::reactive(example_iris_data()),
      "group_data" = shiny::reactive(example_iris_group_data()),
      "feature_data" = shiny::reactive(examplre_iris_feature_data())
    ),
    {
      expect_type(feature_choice_list(), "list")
      expect_type(output$feature_selection_ui, "list")
      session$setInputs("feature_choice" = "Sepal.Length")
      expect_type(distplot_data(), "list")
      expect_type(output$distplot, "character")
      expect_error(
        distplot_eventdata(),
        regexp = "Click on above plot.",
        class = c("shiny.silent.error")
      )
    }
  )
})
