#' Cohort Selection Server
#'
#' @param id A shiny ID
#' @param default_datasets A Shiny reactive that return a list of dataset names
#' @param default_group A Shiny reactive that returns the name of a group
#' @param dataset_type A shiny reactive that returns one of c("analysis", "ici")
#' @param ... unused args
#'
#' @export
cohort_selection_server <- function(
  id,
  default_datasets = shiny::reactive("TCGA"),
  default_group    = shiny::reactive("Immune_Subtype"),
  dataset_type     = shiny::reactive("analysis"),
  ...
){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      display_cohort_mode_choice <- shiny::reactive({
        dataset_type() == "analysis"
      })

      output$display_cohort_mode_choice <- shiny::reactive({
        display_cohort_mode_choice()
      })

      shiny::outputOptions(
        output,
        "display_cohort_mode_choice",
        suspendWhenHidden = FALSE
      )

      cohort_obj_manual <- cohort_manual_selection_server(
        "cohort_manual_selection",
        default_datasets = default_datasets,
        default_group    = default_group,
        dataset_type     = dataset_type
      )

      cohort_obj_upload <- cohort_upload_selection_server(
        "cohort_upload_selection"
      )

      cohort_obj <- shiny::reactive({
        if(is.null(input$cohort_mode_choice)) cohort_mode <- "Cohort Selection"
        else cohort_mode <- input$cohort_mode_choice

        if (cohort_mode == "Cohort Selection") {
          shiny::req(cohort_obj_manual())
          return(cohort_obj_manual())
        } else if (cohort_mode == "Cohort Upload") {
          shiny::req(cohort_obj_upload())
          return(cohort_obj_upload())
        } else {
          stop("Unrecognized cohort creation opion")
        }
      })

      # group key ---------------------------------------------------------------

      group_key_tbl <- shiny::reactive({
        shiny::req(cohort_obj())
        cohort_obj()$group_tbl %>%
          dplyr::select(
            "Name" = .data$short_name,
            "Dataset" = .data$dataset_display,
            "Description" = .data$long_name,
            "Size" = .data$size,
            "Characteristics" = .data$characteristics,
            "Plot Color" = .data$color
          )
      })

      data_table_server(
        "sg_table",
        group_key_tbl,
        options = list(
          dom = "tip",
          pageLength = 10,
          columnDefs = list(
            list(width = '50px',
                 targets = c(1)
            )
          )
        ),
        color = T,
        color_column = "Plot Color",
        colors = ""
      )

      # return ------------------------------------------------------------------
      return(cohort_obj)
    }
  )
}

