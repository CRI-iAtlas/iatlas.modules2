test_that("is_group_filter_valid", {
  expect_false(is_group_filter_valid(NULL))
  expect_false(is_group_filter_valid(list("group_choices" = NULL)))
  expect_false(is_group_filter_valid(list("group_choices" = "C1")))
  expect_false(
    is_group_filter_valid(list(
      "group_choices" = "C1",
      "group_type" = "tag",
      "parent_group_choice" = NULL
    ))
  )
  expect_true(
    is_group_filter_valid(list(
      "group_choices" = "C1",
      "group_type" = "tag",
      "parent_group_choice" = "Immnune_Subtype"
    ))
  )
  expect_true(
    is_group_filter_valid(list(
      "group_choices" = list("C1", "C2"),
      "group_type" = "tag",
      "parent_group_choice" = "Immnune_Subtype"
    ))
  )
})

test_that("get_valid_group_filters", {
  invalid1 <- list()
  invalid2 <- list(NULL)
  valid1 <- list(
    "group_choices" = "C1",
    "group_type" = "tag",
    "parent_group_choice" = "Immnune_Subtype"
  )
  valid2 <- list(
    "group_choices" = "BRCA",
    "group_type" = "tag",
    "parent_group_choice" = "TCGA_Study"
  )
  expect_equal(get_valid_group_filters(invalid1), list())
  expect_equal(get_valid_group_filters(invalid2), list())
  expect_equal(
    get_valid_group_filters(list(
      "element1" = valid1
    )),
    list(valid1)
  )
  expect_equal(
    get_valid_group_filters(list(
      "element1" = valid1,
      "element2" = invalid1
    )),
    list(valid1)
  )
  expect_equal(
    get_valid_group_filters(
      list(
        "element1" = valid1,
        "element2" = valid2
      )
    ),
    list(valid1, valid2)
  )
})

test_that("get_group_filtered_samples", {
  filter_obj <- list(
    list(
      "group_choices" = c("C5", "C6"),
      "parent_group_choice" = "Immnune_Subtype"
    ),
    list(
      "group_choices" = c("COAD", "STAD"),
      "parent_group_choice" = "TCGA_Study"
    )
  )
  result1 <- get_group_filtered_samples(filter_obj, get_tcga_samples(), "TCGA")
  expect_type(result1, "character")
  expect_true(length(result1) > 0)
})
