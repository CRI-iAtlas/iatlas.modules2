### cohort as input -----------------------------------------------------------
## extract from cohort --------------------------------------------------------

#' Get Cohort Feature Class List
#'
#' @param cohort_object A named list
#'
#' @export
get_cohort_feature_class_list <- function(cohort_object){
  cohort_object %>%
    purrr::pluck("feature_tbl") %>%
    dplyr::pull("class") %>%
    unique() %>%
    sort()
}

## check cohort for module display --------------------------------------------

#' Cohort Has Classes
#'
#' @param cohort_object A named list
#' @param classes A vector of strings
#' @param all_classes A logical
#'
#' @export
cohort_has_classes <- function(cohort_object, classes, all_classes = T){
  cohort_classes <- cohort_object %>%
    purrr::pluck("feature_tbl") %>%
    dplyr::pull(.data$class)
  if(all_classes) has_function <- base::all
  else has_function <- base::any
  has_function(classes %in% cohort_classes)
}

#' Cohort Has Features
#'
#' @param cohort_object A named list
#' @param features A vector of strings
#' @param all_features A logical
#'
#' @export
cohort_has_features <- function(cohort_object, features, all_features = T){
  cohort_features <- cohort_object %>%
    purrr::pluck("feature_tbl") %>%
    dplyr::pull(.data$name)
  if(all_features) has_function <- base::all
  else has_function <- base::any
  has_function(features %in% cohort_features)
}

#' Cohort Has Dataset
#'
#' @param cohort_object A named list
#' @param datasets A vector of strings
#'
#' @export
cohort_has_dataset <- function(cohort_object, datasets){
  if(is.null(cohort_object$dataset)) return(F)
  cohort_object$dataset %in% datasets
}

#' Cohort Has Group
#'
#' @param cohort_object A named list
#' @param groups A vector of strings
#'
#' @export
cohort_has_group <- function(cohort_object, groups){
  if(is.null(cohort_object$group_name)) return(F)
  cohort_object$group_name %in% groups
}

## api queries ----------------------------------------------------------------
# features --------------------------------------------------------------------

#' Query Feature Values With Cohort Object
#'
#' @param cohort_object A named list
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param groups A vector of strings
#'
#' @export
query_feature_values_with_cohort_object <- function(
  cohort_object,
  features = NA,
  feature_classes = NA,
  groups = NA
){
  if (cohort_object$group_type == "tag"){
    cohort <-stringr::str_c(
        cohort_object$dataset,
        cohort_object$group_name,
        sep = "_"
      )
  } else if (cohort_object$group_type == "clinical"){
    cohort <-stringr::str_c(
      cohort_object$dataset,
      stringr::str_to_title(cohort_object$group_name),
      sep = "_"
    )
  } else {
      cohort <- cohort_object$dataset
  }
  tbl <- iatlas.api.client::query_feature_values(
    cohort = cohort,
    features = features,
    feature_classes = feature_classes
  ) %>%
    dplyr::filter(.data$sample %in% cohort_object$sample_tbl$sample)
  if(!is.na(groups)){
    group_samples <- cohort_object$sample_tbl %>%
      dplyr::filter(.data$group %in% groups) %>%
      dplyr::pull("sample")
    tbl <- dplyr::filter(tbl, .data$sample %in% group_samples)
  }
  return(tbl)
}

# genes -----------------------------------------------------------------------


#' Query Gene Expression With Cohort Object
#'
#' @param cohort_object A named list
#' @param gene_types A vector of strings
#' @param entrez A vector of integers
#'
#' @export
query_gene_expression_with_cohort_object <- function(
  cohort_object,
  gene_types = NA,
  entrez = NA
){
  if (cohort_object$group_type == "tag"){
    cohort <-stringr::str_c(
      cohort_object$dataset,
      cohort_object$group_name,
      sep = "_"
    )
  } else if (cohort_object$group_type == "clinical"){
    cohort <-stringr::str_c(
      cohort_object$dataset,
      stringr::str_to_title(cohort_object$group_name),
      sep = "_"
    )
  } else {
    cohort <- cohort_object$dataset
  }

  iatlas.api.client::query_gene_expression(
    cohort = cohort,
    gene_types = gene_types,
    entrez = entrez
  ) %>%
    dplyr::filter(.data$sample %in% cohort_object$sample_tbl$sample)

}
