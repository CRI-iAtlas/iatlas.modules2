test_data_dir  <- system.file("test_data", package = "iatlas.modules")

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
      "eventdata" = shiny::reactive(dplyr::tibble("key" = c("setosa", "setosa")))
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

      expect_null(x_feature_input)
      expect_null(y_feature_input)
      expect_true(display_feature_selection_ui())

      expect_type(output$x_feature_selection_ui, "list")
      session$setInputs("x_feature_choice" = "Petal.Length")
      expect_type(output$y_feature_selection_ui, "list")
      session$setInputs("y_feature_choice" = "Petal.Width")
      expect_type(formatted_scatterplot_data(), "list")
      expect_named(formatted_scatterplot_data(), c("x", "y", "text"))

      session$setInputs("x_feature_choice" = "not_a_feature_x")
      expect_error(formatted_scatterplot_data())

    }
  )
})

test_that("drilldown_scatterplot_server_group_set_features", {

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
      "x_feature" = shiny::reactive("Sepal.Length"),
      "y_feature" = shiny::reactive("Sepal.Width")
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
      expect_equal(x_feature_input(), "Sepal.Length")
      expect_equal(y_feature_input(), "Sepal.Width")
      expect_false(display_feature_selection_ui())

      expect_type(formatted_scatterplot_data(), "list")
      expect_named(formatted_scatterplot_data(), c("x", "y", "text"))
    }
  )
})
