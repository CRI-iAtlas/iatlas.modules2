

# used in cohort selection ----------------------------------------------------

#' Numeric Filter Element Server
#'
#' @param id A shiny ID
#' @param reactive_values a shiny::reactiveValues() object
#' @param module_id A unique value for this instance of the element being called
#' @param numeric_named_list A named list
#' @param dataset A string
#'
#' @export
numeric_filter_element_server <- function(
  id,
  reactive_values,
  module_id,
  numeric_named_list,
  dataset
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$select_ui <- shiny::renderUI({
        shiny::req(numeric_named_list())
        shiny::selectInput(
          inputId = ns("numeric_choice"),
          label = "Select or Search for feature",
          choices = numeric_named_list()
        )
      })

      features_tbl <- shiny::reactive({
        shiny::req(dataset(), input$numeric_choice)
        tbl <-
          iatlas.api.client::query_features_range(
            cohorts = dataset(),
            features = input$numeric_choice
          ) %>%
          dplyr::distinct()
      })

      feature_min <- shiny::reactive({
        shiny::req(features_tbl())
        features_tbl() %>%
          dplyr::pull("value_min") %>%
          round(2)
      })

      feature_max <- shiny::reactive({
        shiny::req(features_tbl())
        features_tbl() %>%
          dplyr::pull("value_max") %>%
          round(2)
      })

      output$slider_ui <- shiny::renderUI({
        shiny::req(feature_max(), feature_min())

        shiny::sliderInput(
          inputId = ns("range"),
          label = "Filter:",
          min = feature_min(),
          max = feature_max(),
          value = c(feature_min(), feature_max())
        )
      })

      update_reactive <- shiny::reactive({
        shiny::req(input$numeric_choice, input$range)
        list(input$numeric_choice, input$range)
      })

      shiny::observeEvent(update_reactive(), {
        obj <- Cohort_Numeric_Filter$new(
          "name" = input$numeric_choice,
          "min" = input$range[[1]],
          "max" = input$range[[2]]
        )
        reactive_values[[module_id]] <- obj
      })

      return(reactive_values)
    }
  )
}

#' Group Filter Element Server
#'
#' @param id A shiny ID
#' @param reactive_values a shiny::reactiveValues() object
#' @param module_id A unique value for this instance of the element being called
#' @param group_named_list A named list
#' @param dataset A string
#'
#' @export
group_filter_element_server <- function(
  id,
  reactive_values,
  module_id,
  group_named_list,
  dataset
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$select_ui <- shiny::renderUI({
        shiny::req(group_named_list())
        shiny::selectInput(
          inputId = ns("parent_group_choice"),
          label = "Select or Search for Group",
          choices = group_named_list()
        )
      })

      group_choices <- shiny::reactive({
        shiny::req(input$parent_group_choice, dataset())
        choices <- build_tag_filter_list(input$parent_group_choice, dataset())
      })

      output$checkbox_ui <- shiny::renderUI({
        shiny::req(group_choices())
        shiny::checkboxGroupInput(
          inputId = ns("group_choices"),
          label = "Select choices to include:",
          choices = group_choices(),
          inline = T
        )
      })

      shiny::observeEvent(input$parent_group_choice, {
        reactive_values[[module_id]]$parent_group_choice <- input$parent_group_choice
      })

      shiny::observeEvent(input$group_choices, {
        reactive_values[[module_id]]$group_choices <- input$group_choices
      })

      return(reactive_values)
    }
  )
}

# used in driver module -------------------------------------------------------

#' Numerical Model Covariate Element Server
#'
#' @param id A shiny ID
#' @param reactive_values a shiny::reactiveValues() object
#' @param module_id A unique value for this instance of the element being called
#' @param covariate_list A named list
#'
#' @export
numeric_model_covariate_element_server <- function(
  id,
  reactive_values,
  module_id,
  covariate_list
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$select_covariate_ui <- shiny::renderUI({
        shiny::req(covariate_list())
        shiny::selectInput(
          inputId = ns("covariate_choice_name"),
          label   = "Select or Search for Covariate",
          choices = covariate_list()
        )
      })

      output$select_transformation_ui <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("transformation_choice"),
          label   = "Select or Search for Transformation",
          choices = c("None", "Squared", "Log10", "Reciprocal")
        )
      })

      shiny::observeEvent(input$covariate_choice_name, {
        reactive_values[[module_id]]$covariate_choice_name <-
          input$covariate_choice_name
      })

      shiny::observeEvent(input$transformation_choice, {
        reactive_values[[module_id]]$transformation_choice <-
          input$transformation_choice
      })

      return(reactive_values)
    }
  )
}

#' Categorical Model Covariate Element Server
#'
#' @param id A shiny ID
#' @param reactive_values a shiny::reactiveValues() object
#' @param module_id A unique value for this instance of the element being called
#' @param covariate_list A named list
#'
#' @export
categorical_model_covariate_element_server <- function(
  id,
  reactive_values,
  module_id,
  covariate_list
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$select_covariate_ui <- shiny::renderUI({
        shiny::req(covariate_list())
        shiny::selectInput(
          inputId = ns("covariate_choice"),
          label   = "Select or Search for Covariate",
          choices = covariate_list()
        )
      })

      shiny::observeEvent(input$covariate_choice, {
        reactive_values[[module_id]]$covariate_choice <- input$covariate_choice
      })

      return(reactive_values)
    }
  )
}
