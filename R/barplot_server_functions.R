display_barplot_feature_class_selection_ui <- function(plot_data){
  "feature_class" %in% colnames(plot_data) &&
    (plot_data$feature_class %>% unique() %>% length()) > 1
}

build_barplot_data <- function(plot_data, feature_class_choice){
  if(!is.null(feature_class_choice)){
    barplot_data <- plot_data %>%
      dplyr::filter(.data$feature_class %in% feature_class_choice)
  } else {
    barplot_data <- plot_data
  }

  dplyr::select(
    barplot_data,
    "sample",
    "group",
    "feature_value",
    "feature"
  )
}
