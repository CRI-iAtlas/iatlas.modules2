
test_that("univariate_driver_server", {
  shiny::testServer(
    univariate_driver_server,
    args = list(
      "cohort_obj" = shiny::reactiveVal(get_tcga_immune_subtype_cohort_obj_small())
    ),
    {
      session$setInputs("response_choice" = "leukocyte_fraction")
      session$setInputs("min_wt" = 30)
      session$setInputs("min_mut" = 30)
      session$setInputs("mock_event_data" = data.frame(
        "key" = "C4; PIK3CA:(NS)"
      ))

      expect_type(response_option_list(), "list")
      expect_type(output$response_option_ui, "list")
      expect_equal(response_variable_display(), "Leukocyte Fraction")
      expect_type(tags(), "character")
      expect_true(length(tags()) == 6)
      expect_type(volcano_plot_tbl(), "list")
      expect_named(
        volcano_plot_tbl(),
        c(
          "label",
          "p_value",
          "log10_p_value",
          "log10_fold_change",
          "group",
          "entrez",
          "mutation_code"
        )
      )
      expect_type(total_associations(), "integer")
      expect_type(p_tested(), "double")
      expect_type(output$result_text, "character")
      expect_type(output$volcano_plot, "character")
      expect_type(selected_volcano_result(), "list")
      expect_named(
        selected_volcano_result(),
        c(
          "label",
          "p_value",
          "log10_p_value",
          "log10_fold_change",
          "group",
          "entrez",
          "mutation_code"
        )
      )

      expect_type(violin_tbl(), "list")
      expect_named(violin_tbl(), c("x", "y"))
      expect_true(nrow(violin_tbl()) > 0)

      expect_type(output$violin_plot, "character")
    }
  )
})

