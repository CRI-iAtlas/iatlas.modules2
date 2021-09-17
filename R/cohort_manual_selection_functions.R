# build_cohort_object_from_objects <- function(
#   group_object,
#   filter_object,
#   feature_tbl,
#   sample_tbl
# ){
#   arg_list <- c(
#     group_object,
#     list(
#       "feature_tbl" = feature_tbl,
#       "sample_tbl" = sample_tbl,
#       "samples" = filter_object$samples,
#       "filters" = filter_object
#     )
#   )
#   purrr::invoke(build_cohort_object, arg_list)
# }
#
# build_cohort_object <- function(
#   dataset,
#   sample_tbl,
#   samples,
#   feature_tbl,
#   group_name,
#   group_display,
#   group_type,
#   mutation_id = NA,
#   bin_immune_feature = NA,
#   bin_number = NA,
#   filters = NA
# ){
#
#   # tag types
#   if(group_type == "tag"){
#     cohort_object <- build_tag_cohort_object(
#       dataset = dataset,
#       sample_tbl = sample_tbl,
#       samples = samples,
#       tag_name =  group_name,
#       tag_display = group_display
#     )
#
#   # custom types
#   } else if(group_type == "custom"){
#     if(group_name == "Driver Mutation"){
#       cohort_object <- build_mutation_cohort_object(
#         dataset, samples, mutation_id
#       )
#     } else if (group_name == "Immune Feature Bins") {
#       cohort_object <- build_feature_bin_cohort_object(
#         dataset, samples, bin_immune_feature, bin_number, feature_tbl
#       )
#     } else {
#       stop(group_name, " is not an allowed custom group name.")
#     }
#   } else {
#     stop(group_type, " is not an allowed group type.")
#   }
#
#   cohort_object$feature_tbl <- feature_tbl
#   cohort_object$dataset <- dataset
#   cohort_object$dataset_display <-
#     iatlas.api.client::query_datasets() %>%
#     dplyr::filter(.data$name == dataset) %>%
#     dplyr::pull("display")
#
#   cohort_object$group_type <- group_type
#   cohort_object$filters <- filters
#   cohort_object$plot_colors <- cohort_object$group_tbl %>%
#     dplyr::select("group", "color") %>%
#     tibble::deframe(.)
#   return(cohort_object)
# }
#
# # tag choice ------------------------------------------------------------------
# build_tag_cohort_object <- function(
#   dataset,
#   sample_tbl,
#   samples,
#   tag_name,
#   tag_display
# ){
#   sample_tbl <- sample_tbl %>%
#     dplyr::filter(.data$sample_name %in% samples) %>%
#     dplyr::select(
#       "name" = "tag_long_display",
#       "group" = "tag_short_display",
#       "characteristics" = "tag_characteristics",
#       "color" = "tag_color",
#       "sample" = "sample_name"
#     )
#   group_tbl <- sample_tbl %>%
#     dplyr::group_by(dplyr::across(c(-"sample"))) %>%
#     dplyr::count(name = "size") %>%
#     dplyr::ungroup() %>%
#     dplyr::arrange(.data$group) %>%
#     add_plot_colors_to_tbl(.) %>%
#     dplyr::select("name", "group", "characteristics", "color", "size")
#   sample_tbl <- sample_tbl %>%
#     dplyr::select("sample", "group") %>%
#     dplyr::arrange(.data$sample)
#
#   list(
#     "sample_tbl"    = sample_tbl,
#     "group_tbl"     = group_tbl,
#     "group_name"    = tag_name,
#     "group_display" = tag_display
#
#   )
# }
#
# # mutation choice -------------------------------------------------------------
#
# build_mutation_cohort_object <- function(dataset, samples, mutation){
#
#   sample_tbl <-
#     iatlas.api.client::query_mutation_statuses(
#       mutations = mutation,
#       samples = samples
#     ) %>%
#     dplyr::select("sample" = "sample_name", "group" = "mutation_status")
#
#   group_tbl <- create_mutation_cohort_group_tbl(sample_tbl, mutation) %>%
#     add_plot_colors_to_tbl()
#
#   group_name <- stringr::str_c("Mutation Status: ", mutation)
#   list(
#     "sample_tbl"    = sample_tbl,
#     "group_tbl"     = group_tbl,
#     "group_name"    = group_name,
#     "group_display" = group_name
#   )
# }
#
# create_mutation_cohort_group_tbl <- function(sample_tbl, mutation){
#   sample_tbl %>%
#     dplyr::group_by(.data$group) %>%
#     dplyr::summarise(size = dplyr::n(), .groups = "drop") %>%
#     dplyr::mutate(
#       "name" = mutation,
#       "characteristics" = "Mutation Status"
#     ) %>%
#     dplyr::arrange(.data$group)
# }
#
# # immune feature bin choice ---------------------------------------------------
# build_feature_bin_cohort_object <- function(
#   dataset,
#   samples,
#   feature_name,
#   bin_number,
#   feature_tbl
# ){
#
#   feature_display <- feature_tbl %>%
#     dplyr::filter(.data$name == feature_name) %>%
#     dplyr::pull("display")
#
#   sample_tbl <- build_feature_bin_sample_tbl(
#     dataset,
#     samples,
#     feature_name,
#     bin_number
#   )
#
#   group_tbl <- build_feature_bin_group_tbl(sample_tbl, feature_display) %>%
#     add_plot_colors_to_tbl()
#   group_name  = stringr::str_c("Immune Feature Bins:", feature_display)
#
#   list(
#     "sample_tbl"    = sample_tbl,
#     "group_tbl"     = group_tbl,
#     "group_name"    = group_name,
#     "group_display" = group_name
#   )
# }
#
# build_feature_bin_sample_tbl <- function(
#   dataset, samples, feature_name, n_bins
# ){
#   res <-
#     iatlas.api.client::query_feature_values(
#       features = feature_name, cohorts = dataset
#     ) %>%
#     dplyr::filter(.data$sample %in% samples) %>%
#     dplyr::mutate("group" = as.character(cut(.data$feature_value, n_bins))) %>%
#     dplyr::select("sample", "group")
# }
#
# build_feature_bin_group_tbl <- function(sample_tbl, feature_name){
#   sample_tbl %>%
#     dplyr::group_by(.data$group) %>%
#     dplyr::summarise(size = dplyr::n()) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(
#       "name" = feature_name,
#       "characteristics" = "Immune feature bin range"
#     ) %>%
#     dplyr::arrange(.data$group)
# }
#
# various ---------------------------------------------------------------------

add_plot_colors_to_tbl <- function(tbl){
  if ("color" %in% colnames(tbl)){
    if(any(is.na(tbl$color))) {
      tbl <- dplyr::select(tbl, -"color")
    } else {
      return(tbl)
    }
  }
  tbl <- tbl %>%
    dplyr::select("group") %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$group) %>%
    dplyr::mutate("color" = viridisLite::viridis(dplyr::n())) %>%
    dplyr::inner_join(tbl, ., by = "group")
  return(tbl)
}
