
#' Barplot UI
#'
#' @param id Module ID
#' @param title A string
#'
#' @export
barplot_ui <- function(id, title = ""){

  ns <- shiny::NS(id)

  shiny::tagList(
    titleBox(title),
    shiny::fluidRow(
      shiny::uiOutput(ns("feature_class_selection_ui")),
      plotBox(
        width = 12,
        "barplot" %>%
          ns() %>%
          plotly::plotlyOutput(.) %>%
          shinycssloaders::withSpinner(.),
        plotly_ui(ns("barplot"))
      )
    ),
    shiny::uiOutput(ns("drilldown_ui"))
  )
}
