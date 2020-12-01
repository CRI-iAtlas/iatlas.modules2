build_barplot_data <- function(plot_data_function, feature_class_choice){
  data <-
    plot_data_function(.feature_class = feature_class_choice) %>%
    dplyr::select(dplyr::any_of(
      c("sample", "feature", "feature_value", "group", "group_description")
    ))
}

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

get_barplot_group_data <- function(barplot_data){
  barplot_data %>%
    dplyr::select("group", "description" = "group_description") %>%
    dplyr::distinct()
}
