test_that("drilldown_scatterplot_server", {

  shiny::testServer(
    drilldown_scatterplot_server,
    args = list(
      "plot_data" = shiny::reactive(
        dplyr::select(
          example_iris_data(),
          "sample",
          "x" = "group",
          "y" = "feature_value",
          "color" = "feature"
        )
      ),
      "eventdata" = shiny::reactive(dplyr::tibble("x" = c("setosa", "setosa")))
    ),
    {
      expect_equal(selected_group(), "setosa")
      expect_type(scatterplot_data(), "list")
      expect_type(output$feature_selection_ui, "list")
      session$setInputs("x_feature_choice" = "Petal.Length")
      session$setInputs("y_feature_choice" = "Petal.Width")
      expect_type(formatted_scatterplot_data(), "list")
      expect_named(formatted_scatterplot_data(), c("x", "y", "text"))
    }
  )
})
