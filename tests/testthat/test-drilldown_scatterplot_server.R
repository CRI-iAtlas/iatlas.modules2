test_that("drilldown_scatterplot_server_2_features", {

  shiny::testServer(
    drilldown_scatterplot_server,
    args = list(
      "plot_data" = shiny::reactive(
        dplyr::select(
          example_iris_data(),
          "sample",
          "group",
          "feature_value",
          "feature"
        ) %>%
          dplyr::filter(feature %in% c("Petal.Width", "Sepal.Width"))
      ),
      "eventdata" = shiny::reactive(dplyr::tibble("key" = c("setosa", "setosa")))
    ),
    {
      expect_equal(selected_group(), "setosa")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c("sample", "Sepal.Width", "Petal.Width")
      )
      expect_equal(
        scatterplot_feature_columns(),
        c("Sepal.Width", "Petal.Width")
      )
      expect_false(display_feature_selection_ui())
      expect_type(formatted_scatterplot_data(), "list")
      expect_named(formatted_scatterplot_data(), c("x", "y", "text"))
    }
  )
})

test_that("drilldown_scatterplot_server_4_features", {

  shiny::testServer(
    drilldown_scatterplot_server,
    args = list(
      "plot_data" = shiny::reactive(
        dplyr::select(
          example_iris_data(),
          "sample",
          "group",
          "feature_value",
          "feature"
        )
      ),
      "eventdata" = shiny::reactive(dplyr::tibble("key" = c("setosa", "setosa"))),
      "x_default" = shiny::reactive(NULL),
      "y_default" = shiny::reactive(NULL)
    ),
    {
      expect_equal(selected_group(), "setosa")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c("sample", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
      )
      expect_equal(
        scatterplot_feature_columns(),
        c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
      )
      expect_true(display_feature_selection_ui())
      expect_type(output$x_feature_selection_ui, "list")
      session$setInputs("x_feature_choice" = "Petal.Length")
      expect_type(output$y_feature_selection_ui, "list")
      session$setInputs("y_feature_choice" = "Petal.Width")
      expect_type(formatted_scatterplot_data(), "list")
      expect_named(formatted_scatterplot_data(), c("x", "y", "text"))
    }
  )
})
