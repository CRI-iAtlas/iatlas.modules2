
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


is_numeric_filter_valid <- function(obj){
  all(
    !is.null(obj),
    !any(
      is.null(obj$name),
      is.null(obj$min),
      is.null(obj$max)
    ),
    all(names(obj) %in% c("name", "min", "max"))
  )
}

get_valid_numeric_filters <- function(filter_obj){
  filter_obj %>%
    purrr::keep(purrr::map_lgl(., is_numeric_filter_valid)) %>%
    unname()
}

get_numeric_filtered_samples <- function(filter_obj, samples, dataset){
  filter_obj %>%
    purrr::transpose(.) %>%
    purrr::map(~unlist(.x)) %>%
    purrr::list_modify("type" = NULL) %>%
    purrr::pmap(., get_filtered_samples_by_feature, dataset) %>%
    purrr::reduce(base::intersect, .init = samples)
}

get_filtered_samples_by_feature <- function(name, min, max, dataset){
  iatlas.api.client::query_feature_values(
    cohort = dataset, features = name, max_value = max, min_value = min
  ) %>%
    dplyr::pull(sample)
}
