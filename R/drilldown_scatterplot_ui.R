
drilldown_scatterplot_ui <- function(id){

  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::conditionalPanel(
      condition = "output.display_feature_selection_ui",
      optionsBox(
        width = 12,
        shiny::column(
          width = 6,
          shiny::uiOutput(ns("x_feature_selection_ui"))
        ),
        shiny::column(
          width = 6,
          shiny::uiOutput(ns("y_feature_selection_ui"))
        )
      ),
      ns = ns
    ),
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
