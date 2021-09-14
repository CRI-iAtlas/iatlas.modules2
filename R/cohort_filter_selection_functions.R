
is_group_filter_valid <- function(obj){
  all(
    !is.null(obj),
    !is.null(obj$group_choices),
    !is.null(obj$parent_group_choice)
  )
}

get_valid_group_filters <- function(filter_obj){
  filter_obj %>%
    purrr::keep(purrr::map_lgl(., is_group_filter_valid)) %>%
    unname()
}

get_group_filtered_samples <- function(filter_obj, samples, dataset){
  filter_obj %>%
    purrr::transpose(.) %>%
    purrr::pluck("group_choices") %>%
    purrr::map(
      .,
      ~iatlas.api.client::query_tag_samples(cohorts = dataset, tags = .x)
    ) %>%
    purrr::map(., dplyr::pull, "sample_name") %>%
    purrr::reduce(base::intersect, .init = samples)
}




