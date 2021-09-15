Cohort_Filters <- R6::R6Class("Cohort_Filters", list(
  numeric_filters = NULL,
  group_filters = NULL,
  initialize = function(numeric_filters, group_filters) {
    if(typeof(numeric_filters) != "environment"){
      stop("numeric_filters must be of type environment (class: Cohort_Numeric_Filters)")
    }
    if(!all(class(numeric_filters) == c("Cohort_Numeric_Filters", "R6"))){
      stop("numeric_filters must be of class Cohort_Numeric_Filters")
    }
    if(typeof(group_filters) != "environment"){
      stop("group_filters must be of type environment (class: Cohort_Numeric_Filters)")
    }
    if(!all(class(group_filters) == c("Cohort_Group_Filters", "R6"))){
      stop("group_filters must be of class Cohort_Group_Filters")
    }
    self$numeric_filters <- numeric_filters
    self$group_filters <- group_filters
  },
  get_samples = function(cohorts, cohort_samples){
    numeric_samples <- self$numeric_filters$get_samples(cohorts)
    group_samples   <- self$group_filters$get_samples(cohorts)
    no_numeric_samples <- all(
      length(numeric_samples) == 1,
      is.na(numeric_samples)
    )
    no_group_samples <- all(
      length(group_samples) == 1,
      is.na(group_samples)
    )

    if(all(no_numeric_samples, no_group_samples)){
      return(cohort_samples)
    } else if (no_numeric_samples){
      return(base::intersect(cohort_samples, group_samples))
    } else if (no_group_samples){
      return(base::intersect(cohort_samples, numeric_samples))
    } else {
      samples <- cohort_samples %>%
        base::intersect(group_samples) %>%
        base::intersect(numeric_samples)
      return(samples)
    }
  }
))

Cohort_Numeric_Filters <- R6::R6Class("Cohort_Numeric_Filters", list(
  filter_list = NULL,
  initialize = function(filter_list) {
    if(typeof(filter_list) != "list"){
      stop("Numeric filter list must must be a list")
    }
    if(all("R6" %in% purrr::map(filter_list, class))){
      stop("Numeric filter list items must must be of class R6")
    }
    if(all("Cohort_Numeric_Filter" %in% purrr::map(filter_list, class))){
      stop("Numeric filter list items must must be of class Cohort_Numeric_Filter")
    }
    self$filter_list <- filter_list
  },
  get_samples = function(cohorts){
    if(length(self$filter_list) == 0) {
      return(NA)
    } else {
      samples_list <- self$filter_list %>%
        purrr::map(., ~.x$get_samples(cohorts)) %>%
        purrr::reduce(base::intersect)
    }
  }
))

Cohort_Group_Filters <- R6::R6Class("Cohort_Group_Filters", list(
  filter_list = NULL,
  initialize = function(filter_list) {
    if(typeof(filter_list) != "list"){
      stop("Group filter list must must be a list")
    }
    if(all("R6" %in% purrr::map(filter_list, class))){
      stop("Group filter list items must must be of class R6")
    }
    if(all("Cohort_Group_Filter" %in% purrr::map(filter_list, class))){
      stop("Group filter list items must must be of class Cohort_Group_Filter")
    }
    self$filter_list <- filter_list
  },
  get_samples = function(cohorts){
    if(length(self$filter_list) == 0) {
      return(NA)
    } else {
      samples_list <- self$filter_list %>%
        purrr::map(., ~.x$get_samples(cohorts)) %>%
        purrr::reduce(base::intersect)
    }
  }
))

Cohort_Numeric_Filter <- R6::R6Class("Cohort_Numeric_Filter", list(
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
      iatlas.api.client::query_feature_values(
        cohorts = cohorts,
        features = self$name,
        max_value = self$max,
        min_value = self$min
    ) %>%
      dplyr::pull("sample")
  }
))


Cohort_Group_Filter <- R6::R6Class("Cohort_Group_Filter", list(
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
      iatlas.api.client::query_tag_samples(
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
