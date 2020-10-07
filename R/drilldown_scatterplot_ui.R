
drilldown_scatterplot_ui <- function(id){

  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::uiOutput(ns("feature_selection_ui")),
    plotBox(
      width = 12,
      "scatterplot" %>%
        ns() %>%
        plotly::plotlyOutput(.) %>%
        shinycssloaders::withSpinner(.),
      plotly_ui(ns("scatterplot"))
    )
  )

}
