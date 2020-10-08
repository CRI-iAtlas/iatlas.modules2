test_that("barplot_server_starwars", {

  shiny::testServer(
    barplot_server,
    args = list(
      "feature_data" = shiny::reactive(example_starwars_data())
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

# test_that("barplot_server_iris", {
#
#   shiny::testServer(
#     barplot_server,
#     args = list(
#       "feature_data" = shiny::reactive(example_iris_data()),
#       "drilldown" = shiny::reactive(T)
#     ),
#     {
#       expect_type(output$feature_class_selection_ui, "list")
#       session$setInputs("feature_class_choice" = "Length")
#       expect_type(barplot_data(), "list")
#       expect_named(barplot_data(), c("sample", "x", "y", "color"))
#       expect_type(summarized_barplot_data(), "list")
#       expect_named(
#         summarized_barplot_data(),
#         c("x", "color", "text", "MEAN", "SE")
#       )
#       expect_equal(barplot_source_name(), "proxy1-barplot")
#       expect_type(output$barplot, "character")
#       expect_error(
#         barplot_eventdata(),
#         regexp = "Click on above barplot.",
#         class = c("shiny.silent.error")
#       )
#       expect_type(group_data(), "list")
#       expect_named(group_data(), c("group", "description"))
#     }
#   )
# })
