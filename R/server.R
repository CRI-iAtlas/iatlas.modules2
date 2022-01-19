
server <- function(input, output, session) {
  cohort_object  <- cohort_selection_server("cohort_selection_module")
  cohort_object2 <- cohort_selection_server(
    "ici_cohort_selection_module",
    default_datasets = shiny::reactive(c("Gide_Cell_2019", "HugoLo_IPRES_2016")),
    default_group    = shiny::reactive("Responder"),
    dataset_type     = shiny::reactive("ici"),
    display_module_availibility_string = shiny::reactive(F)
  )

  output$cohort_text_output <- shiny::renderText({
    shiny::req(cohort_object())
    stringr::str_c(cohort_object()$dataset, " : ", cohort_object()$group_display)
  })

  univariate_driver_server(
    "driver_module",
    cohort_object
  )

}
