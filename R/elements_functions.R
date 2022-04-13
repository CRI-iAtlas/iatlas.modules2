
build_tag_filter_list <- function(parent_tag_name, datasets){
  iatlas.api.client::query_tags(
    cohorts = datasets,
    parent_tags = parent_tag_name
  ) %>%
    dplyr::select("tag_short_display", "tag_name") %>%
    dplyr::arrange(.data$tag_name) %>%
    tibble::deframe(.)
}
