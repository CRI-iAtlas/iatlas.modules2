cohort_dataset_selection_server <- function(
  id,
  default_datasets = shiny::reactive("TCGA"),
  dataset_type     = shiny::reactive("analysis")
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      choices <- shiny::reactive({
        iatlas.api.client::query_datasets(types = dataset_type()) %>%
          dplyr::select("display", "name") %>%
          tibble::deframe(.)
      })

      ui_func <- shiny::reactive({
        shiny::req(dataset_type())
        if(dataset_type() == "analysis") return(shiny::selectInput)
        else return(shiny::checkboxGroupInput)
      })


      output$dataset_selection_ui <- shiny::renderUI({
        shiny::req(ui_func(), choices(), default_datasets())
        ui_func()(
          inputId  = ns("dataset_choices"),
          label    = "Select Datasets",
          choices  = choices(),
          selected = default_datasets()
        )
      })

      output$module_availibility_string <- shiny::renderText({
        shiny::req(input$dataset_choices)
        create_cohort_module_string(
          input$dataset_choices
        )
      })

      return(shiny::reactive(input$dataset_choices))
    }
  )
}
