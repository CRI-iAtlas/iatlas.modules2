test_that("plotly_scatter", {
  p <- iris %>%
    plotly_scatter(
      x_col = "Sepal.Length",
      y_col = "Sepal.Width"
    )
  expect_type(p, "list")
  print(p)
})
