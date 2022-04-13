CohortFilters <- R6::R6Class("CohortFilters", list(
  numeric_filters = NULL,
  group_filters = NULL,
  initialize = function(numeric_filters, group_filters) {
    validate_object_type(numeric_filters, "CohortFilterList")
    validate_object_type(group_filters, "CohortFilterList")
    self$numeric_filters <- numeric_filters
    self$group_filters <- group_filters
  },
  get_samples = function(cohorts){
    numeric_samples <- self$numeric_filters$get_samples(cohorts)
    group_samples   <- self$group_filters$get_samples(cohorts)
    no_numeric_filters <- length(numeric_samples) == 0
    no_group_filters   <- length(group_samples) == 0

    if(all(no_numeric_filters, no_group_filters)) return(list())
    else if (no_numeric_filters) return(group_samples)
    else if (no_group_filters) return(numeric_samples)
    else return(base::intersect(group_samples, numeric_samples))
  },
  get_sample_tbl = function(cohorts){
    samples    <- self$get_samples(cohorts)
    sample_tbl <- iatlasGraphQLClient::query_cohort_samples(cohorts = cohorts)
    if(length(samples) > 0) {
      sample_tbl <- dplyr::filter(sample_tbl, .data$sample_name %in% samples)
    }
    return(sample_tbl)
  }
))

CohortFilterList <- R6::R6Class("CohortFilterList", list(
  filter_list = NULL,
  initialize = function(filter_list, type) {
    if(typeof(filter_list) != "list"){
      stop("Filter list must be of type list")
    }
    if(!type %in% c("numeric", "group")){
      stop("type must be either 'numeric' or 'group'")
    }
    if(type == "numeric") {
      purrr::walk(filter_list, validate_object_type, "CohortNumericFilter")
    } else {
      purrr::walk(filter_list, validate_object_type, "CohortGroupFilter")
    }
    self$filter_list <- filter_list
  },
  get_samples = function(cohorts){
    if(length(self$filter_list) == 0) return(list())
    else {
      samples <-
        purrr::map(self$filter_list, ~.x$get_samples(cohorts)) %>%
        purrr::reduce(base::intersect)
      return(samples)
    }
  },
  filter_sample_tbl = function(sample_tbl, cohorts){
    samples <- self$get_samples(cohorts)
    if(length(samples) > 0) {
      sample_tbl <- dplyr::filter(sample_tbl, .data$sample_name %in% samples)
    }
    return(sample_tbl)
  }
))

CohortNumericFilter <- R6::R6Class("CohortNumericFilter", list(
  name = NULL,
  min = NULL,
  max = NULL,
  initialize = function(name, min, max) {
    validate_filter_name(name)
    validate_filter_min(min)
    validate_filter_max(max)
    if(min > max){
      stop("Numeric Filter min must not be lerger than max")
    }
    self$name <- name
    self$min <- min
    self$max <- max
  },
  get_samples = function(cohorts){
    samples <-
      iatlasGraphQLClient::query_feature_values(
        cohorts = cohorts,
        features = self$name,
        max_value = self$max,
        min_value = self$min
    ) %>%
      dplyr::pull("sample")
  }
))


CohortGroupFilter <- R6::R6Class("CohortGroupFilter", list(
  name = NULL,
  values = NULL,
  initialize = function(name, values) {
    validate_filter_name(name)
    validate_filter_values(values)
    self$name <- name
    self$values <- values
  },
  get_samples = function(cohorts){
    samples <-
      iatlasGraphQLClient::query_tag_samples(
        cohorts = cohorts, tags = self$values
      ) %>%
      dplyr::pull("sample_name")
  }
))

validate_filter_name <- function(name){
  if(length(name) != 1){
    stop("Filter name must be length of 1")
  }
  if(typeof(name) != "character"){
    stop("Filter name must be character")
  }
}

validate_filter_min <- function(min){
  if(length(min) != 1){
    stop("Numeric Filter min must be length of 1")
  }
  if(!typeof(min) %in% c("numeric", "integer", "double")){
    stop("Numeric Filter min must be numeric")
  }
}

validate_filter_max <- function(max){
  if(length(max) != 1){
    stop("Numeric Filter max must be length of 1")
  }
  if(!typeof(max) %in% c("numeric", "integer", "double")){
    stop("Numeric Filter max must be numeric")
  }
}

validate_filter_values <- function(values){
  if(length(values) == 0){
    stop("Categorical filter values must be length of atleast 1")
  }
  if(typeof(values) != "character"){
    stop("Categorical filter values must be of type character")
  }
}
