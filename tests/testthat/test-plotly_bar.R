test_that("plotly_bar_one_group", {
  data <- dplyr::starwars %>%
    dplyr::select(
      "x" = "sex",
      "y" = "height"
    ) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$x) %>%
    dplyr::summarise(
      "y" = mean(.data$y),
      "count" = dplyr::n(),
      .groups = "drop"
    )  %>%
    dplyr::mutate("error" = .data$y / sqrt(.data$count))

  p <- plotly_bar(data)
  expect_type(p, "list")
  print(p)
})

test_that("plotly_bar_two_groups", {
  data <- dplyr::starwars %>%
    dplyr::select(
      "x" = "eye_color",
      "color" = "sex",
      "y" = "height"
    ) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$x, .data$color) %>%
    dplyr::summarise(
      "y" = mean(.data$y),
      "count" = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate("error" = .data$y / sqrt(.data$count))

  p <- plotly_bar(data, color_col = "color", error_col = "error")
  expect_type(p, "list")
  print(p)
})
