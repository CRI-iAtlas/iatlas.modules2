test_that("cohort_group_selection_server_immune_subtype", {
  shiny::testServer(
    cohort_group_selection_server,
    args = list(
      "selected_dataset" = shiny::reactiveVal("TCGA"),
      "default_group" = "Immune_Subtype",
      "features_tbl" = shiny::reactiveVal(get_tcga_features_tbl())
    ),
    {
      expect_type(tag_group_tbl(), "list")
      expect_named(tag_group_tbl(), c("display", "name"))
      expect_type(custom_group_tbl(), "list")
      expect_named(custom_group_tbl(), c("display", "name"))
      expect_type(available_groups_list(), "character")
      expect_named(available_groups_list())
      expect_type(output$select_group_ui, "list")
      expect_type(group_choice(), "character")
      expect_equal(group_choice(), default_group)

      session$setInputs("group_choice" = "Immune_Subtype")
      expect_equal(group_choice(), "Immune_Subtype")
      expect_false(display_driver_mutation_ui())
      expect_false(display_immune_feature_bins_ui())

      group_object <- session$getReturned()()
      expect_type(group_object, "environment")
      expect_equal(class(group_object), c("TagGroup", "R6"))
      expect_equal(group_object$dataset_name, "TCGA")
      expect_equal(group_object$group_name, "Immune_Subtype")
      expect_equal(group_object$group_display, "Immune Subtype")
    }
  )
})

test_that("cohort_group_selection_server_driver_mutation", {
  shiny::testServer(
    cohort_group_selection_server,
    args = list(
      "selected_dataset" = shiny::reactiveVal("TCGA"),
      "default_group" = "Immune_Subtype",
      "features_tbl" = shiny::reactiveVal(get_tcga_features_tbl())
    ),
    {
      session$setInputs("group_choice" = "Driver Mutation")
      session$setInputs("driver_mutation_choice" = 'ABL1:(NS)')
      expect_equal(group_choice(), "Driver Mutation")
      expect_true(display_driver_mutation_ui())
      expect_false(display_immune_feature_bins_ui())

      group_object <- session$getReturned()()
      expect_type(group_object, "environment")
      expect_equal(class(group_object), c("MutationStatusGroup", "R6"))
      expect_equal(group_object$dataset_name, "TCGA")
      expect_equal(group_object$group_name, "Mutation Status: ABL1:(NS)")
      expect_equal(group_object$group_display, "Mutation Status: ABL1:(NS)")
      expect_equal(group_object$mutation_name, "ABL1:(NS)")
    }
  )
})

test_that("cohort_group_selection_server_immune_feature_bin", {
  shiny::testServer(
    cohort_group_selection_server,
    args = list(
      "selected_dataset" = shiny::reactiveVal("TCGA"),
      "default_group" = "Immune_Subtype",
      "features_tbl" = shiny::reactiveVal(get_tcga_features_tbl())
    ),
    {
      session$setInputs("group_choice" = "Immune Feature Bins")
      session$setInputs("bin_immune_feature_choice" = "age_at_diagnosis")
      session$setInputs("bin_number_choice" = 2)
      expect_equal(group_choice(), "Immune Feature Bins")
      expect_false(display_driver_mutation_ui())
      expect_true(display_immune_feature_bins_ui())
      expect_type(feature_bin_list(), "list")
      expect_type(output$select_immune_feature_bins_group_ui, "list")

      group_object <- session$getReturned()()

      expect_type(group_object, "environment")
      expect_equal(class(group_object), c("FeatureBinGroup", "R6"))
      expect_equal(group_object$dataset_name, "TCGA")
      expect_equal(group_object$group_name, "Immune Feature Bins: Age At Diagnosis")
      expect_equal(group_object$group_display, "Immune Feature Bins: Age At Diagnosis")
      expect_equal(group_object$feature_name, "age_at_diagnosis")
      expect_equal(group_object$feature_bins, 2)
    }
  )
})

test_that("cohort_group_selection_server_tcga_clinical", {
  shiny::testServer(
    cohort_group_selection_server,
    args = list(
      "selected_dataset" = shiny::reactiveVal("TCGA"),
      "default_group" = "Immune_Subtype",
      "features_tbl" = shiny::reactiveVal(get_tcga_features_tbl())
    ),
    {
      expect_named(tag_group_tbl(), c("display", "name"))
      expect_true(nrow(tag_group_tbl()) > 0)

      expect_named(custom_group_tbl(), c("display", "name"))
      expect_true(nrow(custom_group_tbl()) > 0)

      expect_type(available_groups_list(), "character")

      session$setInputs("group_choice" = "gender")
      expect_equal(group_choice(), "gender")
      expect_false(display_driver_mutation_ui())
      expect_false(display_immune_feature_bins_ui())

      group_object <- session$getReturned()()
      expect_type(group_object, "environment")
      expect_equal(class(group_object), c("TagGroup", "R6"))
      expect_equal(group_object$dataset_name, "TCGA")
      expect_equal(group_object$group_name, "gender")
      expect_equal(group_object$group_display, "Gender")
    }
  )
})

test_that("cohort_group_selection_server_pcawg_feature_bins", {
  shiny::testServer(
    cohort_group_selection_server,
    args = list(
      "selected_dataset" = shiny::reactiveVal("PCAWG"),
      "default_group" = "Immune_Subtype",
      "features_tbl" = shiny::reactiveVal(get_pcawg_features_tbl())
    ),
    {
      session$setInputs("group_choice" = "Immune Feature Bins")
      session$setInputs("bin_immune_feature_choice" = "age_at_diagnosis")
      session$setInputs("bin_number_choice" = 2)
      expect_equal(group_choice(), "Immune Feature Bins")
      expect_false(display_driver_mutation_ui())
      expect_true(display_immune_feature_bins_ui())

      group_object <- session$getReturned()()
      expect_type(group_object, "environment")
      expect_equal(class(group_object), c("FeatureBinGroup", "R6"))
      expect_equal(group_object$dataset_name, "PCAWG")
      expect_equal(group_object$group_name, "Immune Feature Bins: Age At Diagnosis")
      expect_equal(group_object$group_display, "Immune Feature Bins: Age At Diagnosis")
      expect_equal(group_object$feature_name, "age_at_diagnosis")
      expect_equal(group_object$feature_bins, 2)
    }
  )
})
