utils::globalVariables(".")

# plotly label ----------------------------------------------------------------

#' Add Plotly Label
#'
#' @param tbl A tibble
#' @param title A string
#' @param name Name of a column
#' @param group Name of a column
add_plotly_label <- function(tbl, title, name, group){
  dplyr::mutate(tbl, label = paste0(
    "<b>", title, ":</b> ", {{name}}, " (", {{group}}, ")"
  ))
}

#' Add Plotly Value Label
#'
#' @param tbl A tibble with column label
#' @param cols A vector of strings that are columns in the tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
add_plotly_value_label <- function(tbl, cols){
  tbl %>%
    tidyr::pivot_longer(
      .,
      tidyselect::all_of(cols),
      names_to  = "value_name",
      values_to = "value"
    ) %>%
    dplyr::mutate(value_label = stringr::str_glue(
      "{name}: {value}",
      name = stringr::str_to_upper(.data$value_name),
      value = sprintf("%0.3f", .data$value)
    )) %>%
    dplyr::group_by(.data$label) %>%
    dplyr::mutate(value_label = paste0(
      .data$value_label,
      collapse = "</br>"
    )) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      .,
      names_from = .data$value_name,
      values_from = .data$value
    )
}

#' Create Plotly Label
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
create_plotly_label <- function(tbl, name, group, cols, title){
  tbl %>%
    add_plotly_label(title, {{name}}, {{group}}) %>%
    add_plotly_value_label(tidyselect::all_of(cols)) %>%
    tidyr::unite(
      "label",
      .data$label,
      .data$value_label,
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

# misc ------------------------------------------------------------------------

#' Create Nested Named List
#'
#' @param tbl A tibble with the below columns
#' @param names_col1 A column that will be the names of the top list
#' @param names_col2 A column that will be the names of the nested lists
#' @param values_col A column that will be the values of the nested lists
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
create_nested_named_list <- function(
  tbl,
  names_col1 = "class",
  names_col2 = "display",
  values_col = "feature"
){
  list <- tbl %>%
    dplyr::select(tidyselect::all_of(c(
      n1 = names_col1,
      n2 = names_col2,
      v  = values_col
    ))) %>%
    tidyr::drop_na() %>%
    tidyr::nest(data = c(.data$n2, .data$v)) %>%
    dplyr::mutate(data = purrr::map(.data$data, tibble::deframe)) %>%
    tibble::deframe(.)
  return(list)
}
