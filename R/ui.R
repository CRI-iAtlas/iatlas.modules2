ui <- function() {
  shiny::fluidPage(
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel("Start", shiny::textOutput("cohort_text_output")),
      shiny::tabPanel("Cohort Selection", cohort_selection_ui("module"))
    )
  )
}
