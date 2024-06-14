ui <- function() {
  shiny::fluidPage(
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel("Start", shiny::textOutput("cohort_text_output")),
      shiny::tabPanel("Cohort Selection", cohort_selection_ui("cohort_selection_module")),
      shiny::tabPanel("ICI Cohort Selection", cohort_selection_ui("ici_cohort_selection_module")),
      shiny::tabPanel("scRNAseq Cohort Selection", cohort_selection_ui("scrna_cohort_selection_module")),
      shiny::tabPanel("Driver Associations", univariate_driver_ui("driver_module"))
    )
  )
}
