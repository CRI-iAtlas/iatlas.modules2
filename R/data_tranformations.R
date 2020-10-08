
#' Summarise Barplot Standard Error
#'
#' @param data A dataframe with columns "group", "feature", "feature_value"
#' @param title A string

#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
summarise_barplot_se <- function(data, title){
  data %>%
    dplyr::select("group", "feature", "feature_value") %>%
    tidyr::drop_na() %>%
    dplyr::group_by_at(dplyr::vars("group", "feature")) %>%
    dplyr::summarise(
      "MEAN" = mean(.data$feature_value),
      "SE" = .data$MEAN / sqrt(dplyr::n()),
      .groups = "drop"
    ) %>%
    create_plotly_text(.data$feature, .data$group, c("MEAN", "SE"), title)
}
