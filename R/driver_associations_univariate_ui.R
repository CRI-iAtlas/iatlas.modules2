#' Univariate Driver UI
#'
#' @param id A string, a module id
#'
#' @export
univariate_driver_ui <- function(id){

  ns <- shiny::NS(id)

  shiny::tagList(
    iatlasModules::messageBox(
      width = 12,
      shiny::includeMarkdown(get_markdown_path("driver_single"))
    ),
    shiny::fluidRow(
      iatlasModules::optionsBox(
        width = 12,
        shiny::column(
          width = 4,
          shiny::uiOutput(ns("response_option_ui"))
        ),
        shiny::column(
          width = 4,
          shiny::numericInput(
            ns("min_mut"),
            "Minimum number of mutant samples per group:",
            2,
            min = 2
          )
        ),
        shiny::column(
          width = 4,
          shiny::numericInput(
            ns("min_wt"),
            "Minimum number of wild type samples per group:",
            2,
            min = 2
          )
        )
      )
    ),
    shiny::fluidRow(
      iatlasModules::messageBox(
        width = 12,
        shiny::p(shiny::textOutput(ns("result_text")))
      )
    ),
    shiny::fluidRow(
      iatlasModules::plotBox(
        width = 12,
        "volcano_plot" %>%
          ns() %>%
          plotly::plotlyOutput(.) %>%
          shinycssloaders::withSpinner(.),
        iatlasModules::plotly_ui(ns("volcano_plot"))
      )
    ),
    shiny::fluidRow(
      iatlasModules::plotBox(
        width = 12,
        "violin_plot" %>%
          ns() %>%
          plotly::plotlyOutput(.) %>%
          shinycssloaders::withSpinner(.),
        iatlasModules::plotly_ui(ns("violin_plot"))
      )
    )
  )
}
