
# test_that("cohort_manual_selection_server_pcawg", {
#   shiny::testServer(
#     cohort_manual_selection_server,
#     {
#       session$setInputs("dataset_selection-dataset_choices" = "PCAWG")
#
#       expect_type(default_datasets(), "character")
#       expect_equal(selected_datasets(), "PCAWG")
#       expect_equal(datasets(), "PCAWG")
#       expect_type(dataset_feature_tbl(), "list")
#
#       expect_type(group_object(), "environment")
#       expect_equal(class(group_object()), c("TagGroup", "R6"))
#       expect_equal(group_object()$dataset_names, "PCAWG")
#       expect_equal(group_object()$group_name, "Immune_Subtype")
#       expect_equal(group_object()$group_display, "Immune Subtype")
#
#       expect_equal(class(filter_object()), c("CohortFilters", "R6"))
#
#       cohort_object <- session$getReturned()()
#       expect_equal(class(cohort_object()), c("Cohort", "R6"))
#     }
#   )
# })

test_that("cohort_manual_selection_server_pcawg_gender", {
  shiny::testServer(
    cohort_manual_selection_server,
    {
      session$setInputs("dataset_selection-dataset_choices" = "PCAWG")
      session$setInputs("group_selection-group_choice" = "gender")

      expect_type(default_datasets(), "character")
      expect_equal(selected_datasets(), "PCAWG")
      expect_equal(datasets(), "PCAWG")
      expect_type(dataset_feature_tbl(), "list")

      expect_type(group_object(), "environment")
      expect_equal(class(group_object()), c("TagGroup", "R6"))
      expect_equal(group_object()$dataset_names, "PCAWG")
      expect_equal(group_object()$group_name, "gender")
      expect_equal(group_object()$group_display, "Gender")

      expect_equal(class(filter_object()), c("CohortFilters", "R6"))

      cohort_object <- session$getReturned()()
      expect_equal(class(cohort_object()), c("Cohort", "R6"))
    }
  )
})

# test_that("cohort_manual_selection_server_tcga", {
#   shiny::testServer(
#     cohort_manual_selection_server,
#     {
#       session$setInputs("dataset_selection-dataset_choices" = "TCGA")
#
#       expect_type(default_datasets(), "character")
#       expect_type(selected_datasets(), "character")
#       expect_equal(datasets(), "TCGA")
#       expect_type(dataset_feature_tbl(), "list")
#
#       expect_type(group_object(), "environment")
#       expect_equal(class(group_object()), c("TagGroup", "R6"))
#       expect_equal(group_object()$dataset_names, "TCGA")
#       expect_equal(group_object()$group_name, "Immune_Subtype")
#       expect_equal(group_object()$group_display, "Immune Subtype")
#
#
#       expect_equal(class(filter_object()), c("CohortFilters", "R6"))
#
#       cohort_object <- session$getReturned()()
#       expect_equal(class(cohort_object()), c("Cohort", "R6"))
#     }
#   )
# })
