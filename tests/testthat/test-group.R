test_that("TagGroup", {
  group_obj <- TagGroup$new("TCGA", "Immune_Subtype")
  expect_equal(group_obj$group_name, "Immune_Subtype")
  expect_equal(group_obj$group_display, "Immune Subtype")
})

test_that("FeatureBinGroup", {
  group_obj <- FeatureBinGroup$new("TCGA", "age_at_diagnosis", 2)
  expect_equal(group_obj$group_name, "Immune Feature Bins: Age At Diagnosis")
  expect_equal(group_obj$group_display, "Immune Feature Bins: Age At Diagnosis")
})

test_that("MutationStatusGroup", {
  group_obj <- MutationStatusGroup$new("TCGA", "ABL1:(NS)")
  expect_equal(group_obj$group_name, "Mutation Status: ABL1:(NS)")
  expect_equal(group_obj$group_display, "Mutation Status: ABL1:(NS)")
})
