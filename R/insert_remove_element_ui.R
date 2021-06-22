#' Insert Remove Element UI
#'
#' @param id A shiny ID
#' @param button_label A string
#'
#' @export
insert_remove_element_ui <- function(id, button_label = "Add UI element"){
    ns <- shiny::NS(id)

    shiny::actionButton(ns("add_button"), button_label)
}
