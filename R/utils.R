utils::globalVariables(".")

# plotly text ----------------------------------------------------------------

#' Add Plotly Text
#'
#' @param tbl A tibble
#' @param title A string
#' @param name Name of a column
#' @param group Name of a column
add_plotly_text <- function(tbl, title, name, group){
  dplyr::mutate(tbl, text = paste0(
    "<b>", title, ":</b> ", {{name}}, " (", {{group}}, ")"
  ))
}

#' Add Plotly Value Text
#'
#' @param tbl A tibble with column text
#' @param cols A vector of strings that are columns in the tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
add_plotly_value_text <- function(tbl, cols){
  tbl %>%
    tidyr::pivot_longer(
      .,
      tidyselect::all_of(cols),
      names_to  = "value_name",
      values_to = "value"
    ) %>%
    dplyr::mutate(value_text = stringr::str_glue(
      "{name}: {value}",
      name = stringr::str_to_upper(.data$value_name),
      value = sprintf("%0.3f", .data$value)
    )) %>%
    dplyr::group_by(.data$text) %>%
    dplyr::mutate(value_text = paste0(
      .data$value_text,
      collapse = "</br>"
    )) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      .,
      names_from = .data$value_name,
      values_from = .data$value
    )
}

#' Create Plotly Text
#'
#' @param tbl A tibble
#' @param name A column
#' @param group A column
#' @param cols A vector of strings, which are columns in the tibble
#' @param title A string
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
create_plotly_text <- function(tbl, name, group, cols, title){
  tbl %>%
    add_plotly_text(title, {{name}}, {{group}}) %>%
    add_plotly_value_text(tidyselect::all_of(cols)) %>%
    tidyr::unite(
      "text",
      .data$text,
      .data$value_text,
      sep = "</br></br>"
    )
}

# event data utils -------------------------------------------------------------

#' Get Values from Eventdata Dataframe
#'
#' @param eventdata Eventdata from "plotly_click" plotly::event_data
#' @param col The column to get the values from
#' @importFrom magrittr %>%
get_values_from_eventdata <- function(eventdata, col = "x"){
  eventdata %>%
    dplyr::as_tibble() %>%
    magrittr::extract2(col) %>%
    unique()
}

#' Create Group Text from Eventdata Dataframe
#'
#' @param eventdata Eventdata from "plotly_click" plotly::event_data
#' @param group_tbl A Tibble with columns group, name, characteristics
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
create_group_text_from_eventdata <- function(eventdata, group_tbl){
  selected_group <- get_values_from_eventdata(eventdata)
  group_tbl %>%
    dplyr::filter(.data$group == selected_group) %>%
    dplyr::pull("description")
}
