
test_that("cohort_filter_selection_server_pcawg", {
  shiny::testServer(
    cohort_filter_selection_server,
    args = list(
      "datasets" = shiny::reactiveVal("PCAWG"),
      "features_tbl" = shiny::reactiveVal(get_pcawg_features_tbl())
    ),
    {
      expect_type(group_filter_list(), "character")
      expect_equal(length(group_filter_list()), 3)
      expect_named(group_filter_list())

      expect_type(group_element_module_server(), "closure")
      expect_equal(
        class(cohort_group_filter_obj()),
        c("CohortFilterList", "R6")
      )

      expect_named(feature_tbl(), c("class", "display", "feature"))
      expect_true(nrow(feature_tbl()) > 0)
      expect_type(numeric_named_list(), "list")
      expect_named(numeric_named_list())

      expect_type(numeric_element_module_server(), "closure")
      expect_equal(
        class(cohort_numeric_filter_obj()),
        c("CohortFilterList", "R6")
      )
      filter_obj <- session$getReturned()()
      expect_type(filter_obj, "environment")
      expect_equal(class(filter_obj), c("CohortFilters", "R6"))
    }
  )
})

test_that("cohort_filter_selection_server_tcga", {
  shiny::testServer(
    cohort_filter_selection_server,
    args = list(
      "datasets" = shiny::reactiveVal("TCGA"),
      "features_tbl" = shiny::reactiveVal(get_tcga_features_tbl())
    ),
    {

      expect_type(group_filter_list(), "character")
      expect_equal(length(group_filter_list()), 6)
      expect_named(group_filter_list())

      expect_type(group_element_module_server(), "closure")
      expect_equal(
        class(cohort_group_filter_obj()),
        c("CohortFilterList", "R6")
      )

      expect_named(feature_tbl(), c("class", "display", "feature"))
      expect_true(nrow(feature_tbl()) > 0)
      expect_type(numeric_named_list(), "list")
      expect_named(numeric_named_list())

      expect_type(numeric_element_module_server(), "closure")
      expect_equal(
        class(cohort_numeric_filter_obj()),
        c("CohortFilterList", "R6")
      )
      filter_obj <- session$getReturned()()
      expect_type(filter_obj, "environment")
      expect_equal(class(filter_obj), c("CohortFilters", "R6"))
    }
  )
})
