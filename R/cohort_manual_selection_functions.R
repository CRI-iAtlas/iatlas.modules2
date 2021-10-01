add_plot_colors_to_tbl <- function(tbl){
  if ("color" %in% colnames(tbl)){
    if(any(is.na(tbl$color))) {
      tbl <- dplyr::select(tbl, -"color")
    } else {
      return(tbl)
    }
  }
  tbl <- tbl %>%
    dplyr::select("short_name") %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$short_name) %>%
    dplyr::mutate("color" = viridisLite::viridis(dplyr::n())) %>%
    dplyr::inner_join(tbl, ., by = "short_name")
  return(tbl)
}
