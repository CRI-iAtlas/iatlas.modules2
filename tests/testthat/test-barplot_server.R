test_that("barplot_server_starwars", {

  shiny::testServer(
    barplot_server,
    args = list(
      "plot_data" = shiny::reactive(example_starwars_data())
    ),
    {
      expect_false(display_feature_class_selection_ui())
      expect_type(barplot_data(), "list")
      expect_named(
        barplot_data(),
        c("sample", "group","feature_value", "feature")
      )
      expect_type(summarized_barplot_data(), "list")
      expect_named(
        summarized_barplot_data(),
        c("group", "feature", "text", "MEAN", "SE")
      )
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

test_that("barplot_server_iris", {

  shiny::testServer(
    barplot_server,
    args = list(
      "plot_data" = shiny::reactive(example_iris_data()),
      "drilldown" = shiny::reactive(T)
    ),
    {
      expect_true(display_feature_class_selection_ui())
      expect_type(output$feature_class_selection_ui, "list")
      session$setInputs("feature_class_choice" = "Length")
      expect_type(barplot_data(), "list")
      expect_named(
        barplot_data(),
        c("sample", "group","feature_value", "feature")
      )
      expect_type(summarized_barplot_data(), "list")
      expect_named(
        summarized_barplot_data(),
        c("group", "feature", "text", "MEAN", "SE")
      )
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")
      expect_error(
        barplot_eventdata(),
        regexp = "Click on above barplot.",
        class = c("shiny.silent.error")
      )
      expect_type(group_data(), "list")
      expect_named(group_data(), c("group", "description"))
    }
  )
})
