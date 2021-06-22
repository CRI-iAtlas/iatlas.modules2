# used in cohort selection ----------------------------------------------------

#' Numeric Filter Element UI
#'
#' @param id A shiny ID
#'
#' @export
numeric_filter_element_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("select_ui")),
        shiny::uiOutput(ns("slider_ui"))
    )
}

#' Group Filter Element UI
#'
#' @param id A shiny ID
#'
#' @export
group_filter_element_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("select_ui")),
        shiny::uiOutput(ns("checkbox_ui"))
    )
}

# used in driver module -------------------------------------------------------

#' Numerical Model Covariate Element UI
#'
#' @param id A shiny ID
#'
#' @export
numeric_model_covariate_element_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("select_covariate_ui")),
        shiny::uiOutput(ns("select_transformation_ui"))
    )
}

#' Categorical Model Covariate Element UI
#'
#' @param id A shiny ID
#'
#' @export
categorical_model_covariate_element_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("select_covariate_ui"))
    )
}
