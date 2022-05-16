cohort_dataset_selection_server <- function(
  id,
  default_datasets = shiny::reactive("TCGA"),
  dataset_type     = shiny::reactive("analysis"),
  display_module_availibility_string = shiny::reactive(T)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      choices <- shiny::reactive({
        options <- iatlasGraphQLClient::query_datasets(types = dataset_type()) %>%
          dplyr::select("display", "name") %>%
          tibble::deframe(.)
        if(dataset_type() == "analysis") return(options)
        else return(create_ici_options(options)) #for the ICI Cohort Selection, we have RNA-Seq and Nanostring data. This function returns a list organizing them. 
      })

      output$dataset_selection_ui <- shiny::renderUI({
        shiny::req(choices(), default_datasets())
        
        if(dataset_type() == "analysis"){
          shiny::selectInput(
            inputId  = ns("dataset_choices"),
            label    = "Select Datasets",
            choices  = choices(),
            selected = default_datasets()
          )
        }else{ #ICI Cohort selection will have two dataset columns, one for RNA-Seq and the other for Nanostring datasets
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::checkboxGroupInput(
                inputId  = ns("dataset_choices"),
                label    = "Select Datasets - RNA-Seq data",
                choices  = choices()[[1]], #list RNA-seq datasets
                selected = default_datasets()
              )
            ),
            shiny::column(
              width = 6,
              shiny::checkboxGroupInput(
                inputId  = ns("dataset_choices_ns"),
                label    = "Select Datasets - Nanostring data (only Immunomodulators module)",
                choices  = choices()[[2]], #list nanostring datasets
                selected = default_datasets()
              )
            )
          )
        }
      })
      
      dataset_selection <- shiny::reactive({
        if(dataset_type() == "analysis") return(input$dataset_choices)
        else return(c(input$dataset_choices, input$dataset_choices_ns)) #ICI Cohort selection can have RNA-Seq and Nanostring ds selected
      })

      # This is so that the conditional panel can see the various shiny::reactives
      output$display_module_availibility_string <- shiny::reactive({
        display_module_availibility_string()
      })

      shiny::outputOptions(
        output,
        "display_module_availibility_string",
        suspendWhenHidden = FALSE
      )

      output$module_availibility_string <- shiny::renderText({
        shiny::req(input$dataset_choices)
        create_cohort_module_string(
          input$dataset_choices
        )
      })

      return(shiny::reactive(sort(dataset_selection())))
    }
  )
}
