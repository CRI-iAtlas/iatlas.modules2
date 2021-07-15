test_that("module isn't shown", {
  shiny::testServer(
    app = call_module_server,
    args = list(
      "cohort_obj" = shiny::reactiveVal(get_pcawg_immune_subtype_cohort_obj()),
      "server_function" = univariate_driver_server,
      "ui_function" = univariate_driver_ui,
      "test_function" = shiny::reactiveVal(show_ud_submodule)
    ),
    expr = {
      expect_false(display_module())
      expect_type(output$ui, "list")
    }
  )
})

test_that("module is shown", {
  shiny::testServer(
    app = call_module_server,
    args = list(
      "cohort_obj" = shiny::reactiveVal(get_tcga_immune_subtype_cohort_obj()),
      "server_function" = univariate_driver_server,
      "ui_function" = univariate_driver_ui,
      "test_function" = shiny::reactiveVal(show_ud_submodule)
    ),
    expr = {
      expect_true(display_module())
      expect_type(output$ui, "list")
    }
  )
})
