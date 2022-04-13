test_that("cohort_upload_selection_server", {
  shiny::testServer(
    cohort_upload_selection_server,
    {
      session$setInputs("mock_upload_file" = file.path(
        system.file("csv", package = "iatlas.modules2"),
        "test_user_group.csv"
      ))
      session$setInputs("user_group_choice" = "COAD")

      expect_type(user_group_tbl(), "list")
      expect_type(output$dt, "character")
      expect_type(output$user_group_selection, "list")

      cohort <- session$getReturned()()

      expect_equal(class(cohort), c("UploadCohort", "R6"))
      expect_equal(cohort$dataset_names, "TCGA")
      expect_equal(cohort$dataset_displays, "TCGA")
      expect_equal(cohort$group_type, "upload")
      expect_equal(cohort$cohort_names, "TCGA")
      expect_equal(cohort$group_name, "COAD")
      expect_equal(cohort$group_display , "COAD")

    }
  )
})
