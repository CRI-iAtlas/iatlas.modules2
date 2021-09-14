test_that("cohort_selection_server", {
  shiny::testServer(
    cohort_selection_server,
    {
      session$setInputs("cohort_mode_choice" = "Cohort Selection")
      expect_type(cohort_obj(), "list")
      expect_type(group_key_tbl(), "list")
    }
  )
})
