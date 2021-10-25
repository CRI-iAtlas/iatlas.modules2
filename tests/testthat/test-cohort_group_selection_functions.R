

tag_group_tbl1 <- build_tag_group_tbl("PCAWG")
tag_group_tbl2 <- build_tag_group_tbl(c("Gide_Cell_2019", "HugoLo_IPRES_2016"))

custom_group_tbl1 <- build_custom_group_tbl("PCAWG")
custom_group_tbl2 <- build_custom_group_tbl(c("Gide_Cell_2019", "HugoLo_IPRES_2016"))

available_groups_list1 <- build_cohort_group_list(
  tag_group_tbl1, custom_group_tbl1
)

available_groups_list2 <- build_cohort_group_list(
  tag_group_tbl2, custom_group_tbl2
)

test_that("build_tag_group_tbl", {
  expect_named(tag_group_tbl1, c("display", "name"))
  expect_true(nrow(tag_group_tbl1) > 1)
  expect_named(tag_group_tbl2, c("display", "name"))
  expect_true(nrow(tag_group_tbl2) > 1)
})


test_that("build_custom_group_tbl", {
  expect_named(custom_group_tbl1, c("display", "name"))
  expect_true(nrow(custom_group_tbl1) ==  1)
  expect_named(custom_group_tbl2, c("display", "name"))
  expect_true(nrow(custom_group_tbl2) == 1)
})

test_that("build_cohort_group_list", {
  expect_true(length(available_groups_list1) > 1)
  expect_true(length(available_groups_list2) > 1)
})

