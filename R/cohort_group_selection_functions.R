build_tag_group_tbl <- function(datasets){
  tbl <- iatlasGraphQLClient::query_cohorts(datasets = datasets)
  if(!"tag_name" %in% names(tbl)) {
    return(dplyr::tibble("display" = character(), "name" = character()))
  }
  tbl %>%
    dplyr::filter(!is.na(.data$tag_name)) %>%
    dplyr::group_by(.data$tag_name) %>%
    dplyr::mutate("count" = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$count == length(datasets)) %>%
    dplyr::select("display" = "tag_short_display", "name" = "tag_name") %>%
    dplyr::distinct()
}


build_custom_group_tbl <- function(datasets){
  dplyr::bind_rows(
    dplyr::tribble(
      ~name,                 ~dataset,
      "Immune Feature Bins", "TCGA",
      "Driver Mutation",     "TCGA",
      "Immune Feature Bins", "PCAWG",
    ),
    dplyr::tibble(
      "name" = "Immune Feature Bins",
      "dataset" = iatlasGraphQLClient::query_datasets(types = "ici")$name
    )
  ) %>%
    dplyr::filter(.data$dataset %in% datasets) %>%
    dplyr::group_by(.data$name) %>%
    dplyr::mutate("count" = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$count == length(datasets)) %>%
    dplyr::mutate("display" = .data$name) %>%
    dplyr::select("display", "name") %>%
    dplyr::distinct()
}

build_cohort_group_list <- function(tag_tbl, custom_tbl){
  dplyr::bind_rows(tag_tbl, custom_tbl) %>%
    dplyr::select("display", "name") %>%
    tibble::deframe(.)
}

