
#' Summarise Barplot Standard Error
#'
#' @param data A dataframe with columns "x", "y", "color"
#' @param title A string

#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
summarise_barplot_se <- function(data, title){
  data %>%
    tidyr::drop_na() %>%
    dplyr::group_by_at(dplyr::vars("x", "color")) %>%
    dplyr::summarise(
      "MEAN" = mean(.data$y),
      "SE" = .data$MEAN / sqrt(dplyr::n()),
      .groups = "drop"
    ) %>%
    create_plotly_label(.data$color, .data$x, c("MEAN", "SE"), title) %>%
    dplyr::rename("text" = "label")
}
