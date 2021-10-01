test_that("cohort_dataset_selection_server", {
  shiny::testServer(
    cohort_dataset_selection_server,
    {
      expect_type(output$dataset_selection_ui, "list")
      session$setInputs("dataset_choices" = "PCAWG")
      expect_type(output$module_availibility_string, "character")
      expect_equal(session$getReturned()(), "PCAWG")
      session$setInputs("dataset_choices" = "TCGA")
      expect_equal(session$getReturned()(), "TCGA")
    }
  )
})

test_that("cohort_dataset_selection_server ici", {
  shiny::testServer(
    cohort_dataset_selection_server,
    args = list(
      "dataset_type" = shiny::reactive("ici"),
      "default_datasets" = shiny::reactive(c("Gide_Cell_2019", "HugoLo_IPRES_2016"))
    ),
    {
      expect_type(output$dataset_selection_ui, "list")

      session$setInputs("dataset_choices" = c("Gide_Cell_2019", "HugoLo_IPRES_2016"))
      expect_type(output$module_availibility_string, "character")
      expect_equal(session$getReturned()(), c("Gide_Cell_2019", "HugoLo_IPRES_2016"))

      session$setInputs("dataset_choices" = "HugoLo_IPRES_2016")
      expect_equal(session$getReturned()(), "HugoLo_IPRES_2016")
    }
  )
})
