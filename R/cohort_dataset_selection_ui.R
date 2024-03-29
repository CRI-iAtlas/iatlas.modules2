cohort_dataset_selection_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    iatlas.modules::messageBox(
      width = 12,
      shiny::includeMarkdown(get_markdown_path("cohort_dataset_selection")),
      shiny::conditionalPanel(
        shiny::textOutput(ns("module_availibility_string")),
        condition = "output.display_module_availibility_string",
        ns = ns
      )
    ),
    shiny::fluidRow(
      iatlas.modules::optionsBox(
        width = 12,
        shiny::column(
          width = 12,
          shiny::uiOutput(ns("dataset_selection_ui"))
        )
      ),
    )
  )
}
