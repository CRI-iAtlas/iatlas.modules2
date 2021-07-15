build_custom_group_tbl <- function(.dataset){
  dplyr::tribble(
    ~name,                 ~dataset, ~type,
    "Immune Feature Bins", "TCGA",    "custom",
    "Driver Mutation",     "TCGA",    "custom",
    "Immune Feature Bins", "PCAWG",   "custom",
  ) %>%
    dplyr::filter(.data$dataset == .dataset) %>%
    dplyr::mutate("display" = .data$name) %>%
    dplyr::select("display", "name")
}

build_cohort_group_list <- function(tag_tbl, custom_tbl){
  dplyr::bind_rows(tag_tbl, custom_tbl) %>%
    dplyr::select("display", "name") %>%
    tibble::deframe(.)
}

