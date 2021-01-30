plot_data <- dplyr::select(
  example_iris_data(),
  "sample",
  "group",
  "feature_value",
  "feature"
) %>%
  dplyr::filter(.data$feature %in% c("Petal.Width", "Sepal.Width"))

scatterplot_data  <- build_scatterplot_data(plot_data, "setosa")


test_that("build_scatterplot_data", {
  expect_type(scatterplot_data, "list")
  expect_named(scatterplot_data, c("sample", "Sepal.Width", "Petal.Width"))
})


test_that("format_scatterplot_data", {

  result <- format_scatterplot_data(
    scatterplot_data,
    "Sepal.Width",
    "Petal.Width",
    "group"
  )
  expect_type(result, "list")
  expect_named(result, c("x", "y", "text"))
})
