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
cgfs2 <- CohortFilterList$new(list(cgf2), type = "group")


cf1 <- CohortFilters$new(
  numeric_filters = cnfs1,
  group_filters = cgfs1
)

cf2 <- CohortFilters$new(
  numeric_filters = cnfs1,
  group_filters = cgfs2
)

tag_group <- TagGroup$new(
  dataset_name = "PCAWG", group_name = "Immune_Subtype"
)
mutation_group <- MutationStatusGroup$new(
  dataset_name = "TCGA", mutation_name = "ABL1:(NS)"
)
bin_group <- FeatureBinGroup$new(
  dataset_name = "TCGA", feature_name = "B_cells_Aggregate2", feature_bins = 2
)

test_that("tag cohort", {
  cohort <- Cohort$new(
    "feature_tbl" = get_pcawg_features_tbl(),
    "filter_object" = cf1,
    "group_object" = tag_group
  )

  expect_equal(class(cohort), c("Cohort", "R6"))
})

test_that("mutation cohort", {
  cohort <- Cohort$new(
    "feature_tbl" = get_tcga_features_tbl(),
    "filter_object" = cf2,
    "group_object" = mutation_group
  )

  expect_equal(class(cohort), c("Cohort", "R6"))
})

test_that("feature bin cohort", {
  cohort <- Cohort$new(
    "feature_tbl" = get_tcga_features_tbl(),
    "filter_object" = cf2,
    "group_object" = bin_group
  )

  expect_equal(class(cohort), c("Cohort", "R6"))
})

