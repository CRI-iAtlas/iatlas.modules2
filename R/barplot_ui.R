
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
    messageBox(
      width = 12,
      htmltools::includeMarkdown(
        get_markdown_path("barchart1")
      )
    ),
    shiny::conditionalPanel(
      condition = "output.display_feature_class_selection_ui",
      ns = ns,
      shiny::fluidRow(
        optionsBox(
          width = 12,
          shiny::column(
            width = 12,
            shiny::uiOutput(ns("feature_class_selection_ui"))
          )
        )
      )
    ),
    shiny::fluidRow(
      plotBox(
        width = 12,
        "barplot" %>%
          ns() %>%
          plotly::plotlyOutput(.) %>%
          shinycssloaders::withSpinner(.),
        plotly_ui(ns("barplot"))
      )
    ),
    shiny::conditionalPanel(
      condition = "output.display_drilldown_ui",
      ns = ns,
      drilldown_scatterplot_ui(ns("scatterplot"))
    )
  )
}
