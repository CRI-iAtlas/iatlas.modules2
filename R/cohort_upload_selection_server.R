cohort_upload_selection_server <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <-  session$ns

      user_group_tbl <- shiny::reactive({

        if(!is.null(input$mock_upload_file)){
          upload_file <- input$mock_upload_file
        } else if (!is.null(input$file$datapath)){
          upload_file <- input$file$datapath
        } else {
          upload_file <- NULL
        }
        shiny::validate(shiny::need(
          upload_file,
          "Use above uploader to upload group csv",
        ))

        result <- try(readr::read_csv(upload_file))
        if (tibble::is_tibble(result)) {
          return(result)
        } else {
          return(NA)
        }
      })

      shiny::observeEvent(input$filehelp, {
        shiny::showModal(shiny::modalDialog(
          title = "Formatting custom groups",
          shiny::includeMarkdown(get_markdown_path("user_groups")),
          size = "l",
          easyClose = TRUE
        ))
      })

      output$dt <- DT::renderDataTable({
        shiny::validate(shiny::need(
          nrow(user_group_tbl()) > 0,
          "Use above uploader to upload group csv"
        ))
        user_group_tbl()
      })

      output$user_group_selection <- shiny::renderUI({
        shiny::req(user_group_tbl())
        shiny::selectInput(
          inputId = ns("user_group_choice"),
          label   = "Select or Search for group",
          choices = colnames(user_group_tbl()[-1])
        )
      })

      cohort_obj <- shiny::reactive({

        shiny::req(user_group_tbl(), input$user_group_choice)

        UploadCohort$new(
          "upload_tbl" = user_group_tbl(),
          "group_name" = input$user_group_choice
        )
      })

      return(cohort_obj)
    }
  )
}
