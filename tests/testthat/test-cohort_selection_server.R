test_that("cohort_selection_server", {
  shiny::testServer(
    cohort_selection_server,
    {
      session$setInputs("cohort_mode_choice" = "Cohort Selection")
      expect_type(group_key_tbl(), "list")
      cohort_object <- session$getReturned()()
      expect_equal(class(cohort_object), c("Cohort", "R6"))
    }
  )
})
