test_get_tables <- function(group_obj, samples_tbl){
  tables <- group_obj$get_tables(samples_tbl)
  expect_named(tables, c("sample_tbl", "group_tbl"))
  expect_named(tables$sample_tbl, c("sample_name", "group_name", "dataset_name"))
  expect_named(
    tables$group_tbl,
    c(
      "short_name",
      "long_name",
      "characteristics",
      "color",
      "size",
      "order",
      "dataset_name",
      "dataset_display"
    )
  )
}

test_that("TagGroup TCGA Immune_Subtype", {
  group_obj <- TagGroup$new("TCGA", "Immune_Subtype")
  expect_equal(group_obj$group_name, "Immune_Subtype")
  expect_equal(group_obj$group_display, "Immune Subtype")
  expect_equal(group_obj$dataset_names, "TCGA")
  expect_equal(group_obj$cohort_names, "TCGA_Immune_Subtype")
  test_get_tables(group_obj, get_tcga_immune_subtypes_samples_tbl())
})

test_that("TagGroup PCAWG Gender", {
  group_obj <- TagGroup$new("PCAWG", "gender")
  expect_equal(group_obj$group_name, "gender")
  expect_equal(group_obj$group_display, "Gender")
  expect_equal(group_obj$dataset_names, "PCAWG")
  expect_equal(group_obj$cohort_names, "PCAWG_Gender")
  test_get_tables(group_obj, get_pcawg_gender_samples_tbl())
})

test_that("TagGroup ici", {
  group_obj <- TagGroup$new(c("Gide_Cell_2019", "HugoLo_IPRES_2016"), "Responder")
  expect_equal(group_obj$group_name, "Responder")
  expect_equal(group_obj$group_display, "Responder")
  expect_equal(group_obj$dataset_names, c("Gide_Cell_2019", "HugoLo_IPRES_2016"))
  expect_equal(group_obj$cohort_names, c("Gide_Cell_2019_Responder", "HugoLo_IPRES_2016_Responder"))
  test_get_tables(group_obj, get_ici_samples_tbl())
})

test_that("FeatureBinGroup", {
  group_obj <- FeatureBinGroup$new("TCGA", "age_at_diagnosis", 2)
  expect_equal(group_obj$group_name, "Immune Feature Bins: Age At Diagnosis")
  expect_equal(group_obj$group_display, "Immune Feature Bins: Age At Diagnosis")
  expect_equal(group_obj$dataset_names, "TCGA")
  expect_equal(group_obj$cohort_names, "TCGA")
  test_get_tables(group_obj, get_tcga_samples_tbl())
})

test_that("MutationStatusGroup", {
  group_obj <- MutationStatusGroup$new("TCGA", "ABL1:(NS)")
  expect_equal(group_obj$group_name, "Mutation Status: ABL1:(NS)")
  expect_equal(group_obj$group_display, "Mutation Status: ABL1:(NS)")
  expect_equal(group_obj$dataset_names, "TCGA")
  expect_equal(group_obj$cohort_names, "TCGA")
  test_get_tables(group_obj, get_tcga_samples_tbl())
})
