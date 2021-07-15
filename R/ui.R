ui <- function() {
  shiny::fluidPage(
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel("Start", shiny::textOutput("cohort_text_output")),
      shiny::tabPanel("Cohort Selection", cohort_selection_ui("module")),
      shiny::tabPanel("Driver Associations", univariate_driver_ui("driver_module"))
    )
  )
}
