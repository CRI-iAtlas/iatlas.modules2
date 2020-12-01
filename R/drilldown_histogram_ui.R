
drilldown_histogram_ui <- function(id){

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      messageBox(
        width = 12,
      )
    ),
    shiny::fluidRow(
      plotBox(
        width = 12,
        "histogram" %>%
          ns() %>%
          plotly::plotlyOutput(.) %>%
          shinycssloaders::withSpinner(.),
        plotly_ui(ns("histogram"))
      )
    )
  )
}
