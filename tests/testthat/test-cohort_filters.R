cnf1 <- CohortNumericFilter$new(
  "name" = "B_cells_Aggregate2",
  "min" = 0,
  "max" = .1
)

cnf2 <- CohortNumericFilter$new(
  "name" = "Neutrophils_Aggregate2",
  "min" = 0,
  "max" = .1
)

cnfs1 <- CohortFilterList$new(list(cnf1, cnf2), type = "numeric")

cgf1 <- CohortGroupFilter$new(
  "name" = "PCAWG_Study",
  "values" = c("BLCA-US", "BRCA-US", "CLLE-ES")
)

cgf2 <- CohortGroupFilter$new(
  "name" = "Immune_Subtype",
  "values" = c("C1", "C2", "C3", "C4", "C5")
)

cgfs1 <- CohortFilterList$new(list(cgf1, cgf2), type = "group")

cf1 <- CohortFilters$new(
  numeric_filters = cnfs1,
  group_filters = cgfs1
)

test_that("CohortFilters", {
  expect_equal(cf1$numeric_filters, cnfs1)
  expect_equal(cf1$group_filters, cgfs1)
  expect_equal(class(cf1), c("CohortFilters", "R6"))

  samples <- cf1$get_samples(cohorts = "PCAWG")
  expect_type(samples, "character")

  sample_tbl <- cf1$get_sample_tbl(cohorts = "PCAWG")
  expect_named(sample_tbl, c("cohort_name", "sample_name", "dataset_name", "dataset_display", "tag_name"))
  expect_true(nrow(sample_tbl) > 0)

  sample_tbl <- cf1$get_sample_tbl(cohorts = "PCAWG_PCAWG_Study")
  expect_named(
    sample_tbl,
    c(
      "cohort_name",
      "sample_name",
      "dataset_name",
      "dataset_display",
      "tag_characteristics",
      "tag_color",
      "tag_long_display",
      "tag_name",
      "tag_order",
      "tag_short_display"
    )
  )
  expect_true(nrow(sample_tbl) > 0)
})

test_that("CohortFilterList1", {
  expect_equal(class(cgfs1), c("CohortFilterList", "R6"))
  samples <- cgfs1$get_samples(cohorts = "PCAWG")
  expect_type(samples, "character")
  sample_tbl <- cgfs1$filter_sample_tbl(get_pcawg_study_samples_tbl(), "PCAWG_PCAWG_Study")
  expect_named(
    sample_tbl,
    c(
      "cohort_name",
      "sample_name",
      "dataset_name",
      "dataset_display",
      "tag_characteristics",
      "tag_color",
      "tag_long_display",
      "tag_name",
      "tag_order",
      "tag_short_display"
      )
  )
  expect_true(nrow(sample_tbl) > 0)
})

test_that("CohortFilterList2", {
  expect_equal(class(cnfs1), c("CohortFilterList", "R6"))
  samples <- cnfs1$get_samples(cohorts = "PCAWG")
  expect_type(samples, "character")
  expect_true(length(samples) > 0)
  sample_tbl <- cgfs1$filter_sample_tbl(get_pcawg_study_samples_tbl(), "PCAWG_PCAWG_Study")
  expect_named(
    sample_tbl,
    c(
      "cohort_name",
      "sample_name",
      "dataset_name",
      "dataset_display",
      "tag_characteristics",
      "tag_color",
      "tag_long_display",
      "tag_name",
      "tag_order",
      "tag_short_display"
      )
  )
  expect_true(nrow(sample_tbl) > 0)
})

test_that("CohortNumericFilter", {
  expect_equal(cnf1$name, "B_cells_Aggregate2")
  expect_equal(cnf1$min, 0)
  expect_equal(cnf1$max, .1)
  expect_equal(class(cnf1), c("CohortNumericFilter", "R6"))
  samples <- cnf1$get_samples(cohorts = c("PCAWG", "TCGA"))
  expect_type(samples, "character")
})

test_that("CohortGroupFilter", {
  expect_equal(cgf1$name, "PCAWG_Study")
  expect_equal(cgf1$values, c("BLCA-US", "BRCA-US", "CLLE-ES"))
  expect_equal(class(cgf1), c("CohortGroupFilter", "R6"))
  samples <- cgf1$get_samples(cohorts = c("PCAWG", "TCGA"))
  expect_type(samples, "character")
})
