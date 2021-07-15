tag_group_tbl <- iatlas.api.client::query_dataset_tags("PCAWG") %>%
  dplyr::select("display" = "tag_short_display", "name" = "tag_name")

custom_group_tbl <- build_custom_group_tbl("PCAWG")

available_groups_list <- build_cohort_group_list(
  tag_group_tbl, custom_group_tbl
)

feature_bin_tbl <- "PCAWG" %>%
  iatlas.api.client::query_features() %>%
  dplyr::select("class", "display", "name")

test_that("build_custom_group_tbl", {
  expect_equal(
    custom_group_tbl,
    dplyr::tribble(
      ~display,              ~name,
      "Immune Feature Bins", "Immune Feature Bins"
    )
  )
  result <- build_custom_group_tbl("PCAWG")
  expect_equal(
    result,
    dplyr::tribble(
      ~display,              ~name,
      "Immune Feature Bins", "Immune Feature Bins",
    )
  )
})

test_that("build_cohort_group_list", {
  expect_equal(
    available_groups_list,
    c(
      "Gender" = "gender",
      "Immune Subtype" = "Immune_Subtype",
      "PCAWG Study" = "PCAWG_Study",
      "Immune Feature Bins" = "Immune Feature Bins"
    )
  )
})

