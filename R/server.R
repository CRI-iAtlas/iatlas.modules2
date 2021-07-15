
server <- function(input, output, session) {
  cohort_object <- cohort_selection_server("module")
  output$cohort_text_output <- shiny::renderText({
    shiny::req(cohort_object())
    stringr::str_c(cohort_object()$dataset, " : ", cohort_object()$group_display)
  })
  call_iatlas_module(
    "driver_module",
    driver_associations_server,
    input,
    session,
    cohort_object
  )

}
