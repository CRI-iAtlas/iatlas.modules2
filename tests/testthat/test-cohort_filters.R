cnf1 <- Cohort_Numeric_Filter$new(
  "name" = "B_cells_Aggregate2",
  "min" = 0,
  "max" = .001
)

cnf2 <- Cohort_Numeric_Filter$new(
  "name" = "Neutrophils_Aggregate2",
  "min" = 0,
  "max" = .1
)

cnfs1 <- Cohort_Numeric_Filters$new(list(cnf1, cnf2))

test_that("Cohort_Filters", {
  samples1 <- c("s1", "s2")
  catgegorical_filters1 <- list()
  catgegorical_filters2 <- list(
    list("parent_group_choice" = "pgc", "group_choices" = c("x1", "x2"))
  )

  cf1 <- Cohort_Filters$new(
    samples = samples1,
    numeric_filters = cnfs1,
    catgegorical_filters = catgegorical_filters1
  )
  expect_equal(cf1$samples, samples1)
  expect_equal(cf1$numeric_filters, cnfs1)
  expect_equal(cf1$catgegorical_filters, catgegorical_filters1)
  expect_equal(class(cf1), c("Cohort_Filters", "R6"))


  # cf2 <- Cohort_Filters$new(
  #   samples = samples1,
  #   numeric_filters = cnfs1,
  #   catgegorical_filters = catgegorical_filters2
  # )
})

# test_that("Cohort_Numeric_Filters", {
#   expect_equal(class(cnfs1), c("Cohort_Numeric_Filters", "R6"))
#   samples <- cnfs1$get_samples(cohorts = "PCAWG")
#   expect_type(samples, "character")
# })
#
#
# test_that("Cohort_Numeric_Filter", {
#   expect_equal(cnf1$name, "B_cells_Aggregate2")
#   expect_equal(cnf1$min, 0)
#   expect_equal(cnf1$max, .001)
#   expect_equal(class(cnf1), c("Cohort_Numeric_Filter", "R6"))
#   samples <- cnf1$get_samples(cohorts = c("PCAWG", "TCGA"))
#   expect_type(samples, "character")
# })
