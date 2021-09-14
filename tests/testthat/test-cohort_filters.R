test_that("Cohort_Filters", {
  samples1 <- c("s1", "s2")
  catgegorical_filters1 <- list()
  catgegorical_filters2 <- list(
    list("parent_group_choice" = "pgc", "group_choices" = c("x1", "x2"))
  )
  numeric_filters1 <- list()
  numeric_filters2 <- list(
    list("name" = "n1", "min" = 0, "max" = 1)
  )


  cf1 <- Cohort_Filters$new(
    samples = samples1,
    catgegorical_filters = catgegorical_filters1,
    numeric_filters = numeric_filters1
  )
  expect_equal(cf1$samples, samples1)
  expect_equal(cf1$numeric_filters, numeric_filters1)
  expect_equal(cf1$catgegorical_filters, catgegorical_filters1)
  expect_equal(class(cf1), c("Cohort_Filters", "R6"))


  cf2 <- Cohort_Filters$new(
    samples = samples1,
    numeric_filters = numeric_filters2,
    catgegorical_filters = catgegorical_filters2
  )
})

