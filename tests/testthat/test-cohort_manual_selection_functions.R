test_that("add_plot_colors_to_tbl", {
  tbl1 <- dplyr::tribble(
    ~short_name, ~size, ~name,       ~characteristics,
    "Mut",  2L,    "EZH2:(NS)", "Mutation Status",
    "Wt",   30L,   "EZH2:(NS)", "Mutation Status"
  )
  res1 <- add_plot_colors_to_tbl(tbl1)
  res2 <- add_plot_colors_to_tbl(dplyr::tibble("short_name" = c("C1", "C2", "C3")))
  res3 <- add_plot_colors_to_tbl(dplyr::tibble("short_name" = "C1"))
  expect_named(res1, c("short_name", "size", "name", "characteristics", "color"))
  expect_named(res2, c("short_name", "color"))
  expect_named(res3, c("short_name", "color"))
})

