test_that("distributions_plot_server_iris", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "plot_data_func" = shiny::reactive(example_iris_data_func),
      "feature_classes" = shiny::reactive(c("Length", "Width"))
    ),
    {
      expect_true(display_feature_class_selection_ui())
      expect_type(output$feature_class_selection_ui, "list")
      session$setInputs("feature_class_choice" = "Length")
      # expect_type(feature_choice_list(), "list")
      # expect_type(output$feature_selection_ui, "list")
      # session$setInputs("feature_choice" = "Sepal.Length")
      # expect_type(distplot_data(), "list")
      # expect_type(output$distplot, "character")
      # expect_error(
      #   distplot_eventdata(),
      #   regexp = "Click on above plot.",
      #   class = c("shiny.silent.error")
      # )
    }
  )
})
