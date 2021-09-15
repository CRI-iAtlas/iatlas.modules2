cnf1 <- Cohort_Numeric_Filter$new(
  "name" = "B_cells_Aggregate2",
  "min" = 0,
  "max" = .1
)

cnf2 <- Cohort_Numeric_Filter$new(
  "name" = "Neutrophils_Aggregate2",
  "min" = 0,
  "max" = .1
)

cnfs1 <- Cohort_Numeric_Filters$new(list(cnf1, cnf2))

cgf1 <- Cohort_Group_Filter$new(
  "name" = "PCAWG_Study",
  "values" = c("BLCA-US", "BRCA-US", "CLLE-ES")
)

cgf2 <- Cohort_Group_Filter$new(
  "name" = "Immune_Subtype",
  "values" = c("C1", "C2", "C3", "C4", "C5")
)

cgfs1 <- Cohort_Group_Filters$new(list(cgf1, cgf2))

cf1 <- Cohort_Filters$new(
  numeric_filters = cnfs1,
  group_filters = cgfs1
)



test_that("Cohort_Filters", {
  expect_equal(cf1$numeric_filters, cnfs1)
  expect_equal(cf1$group_filters, cgfs1)
  expect_equal(class(cf1), c("Cohort_Filters", "R6"))
  samples <- cf1$get_samples(cohorts = "PCAWG", cohort_samples = get_pcawg_samples())
  expect_type(samples, "character")
})

test_that("Cohort_Group_Filters", {
  expect_equal(class(cgfs1), c("Cohort_Group_Filters", "R6"))
  samples <- cgfs1$get_samples(cohorts = "PCAWG")
  expect_type(samples, "character")
})

test_that("Cohort_Numeric_Filters", {
  expect_equal(class(cnfs1), c("Cohort_Numeric_Filters", "R6"))
  samples <- cnfs1$get_samples(cohorts = "PCAWG")
  expect_type(samples, "character")
})


test_that("Cohort_Numeric_Filter", {
  expect_equal(cnf1$name, "B_cells_Aggregate2")
  expect_equal(cnf1$min, 0)
  expect_equal(cnf1$max, .1)
  expect_equal(class(cnf1), c("Cohort_Numeric_Filter", "R6"))
  samples <- cnf1$get_samples(cohorts = c("PCAWG", "TCGA"))
  expect_type(samples, "character")
})

test_that("Cohort_Group_Filter", {
  expect_equal(cgf1$name, "PCAWG_Study")
  expect_equal(cgf1$values, c("BLCA-US", "BRCA-US", "CLLE-ES"))
  expect_equal(class(cgf1), c("Cohort_Group_Filter", "R6"))
  samples <- cgf1$get_samples(cohorts = c("PCAWG", "TCGA"))
  expect_type(samples, "character")
})
