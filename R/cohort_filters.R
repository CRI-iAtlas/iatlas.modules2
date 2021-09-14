Cohort_Filters <- R6::R6Class("Cohort_Filters", list(
  samples = NULL,
  numeric_filters = NULL,
  catgegorical_filters = NULL,
  initialize = function(samples, numeric_filters, catgegorical_filters = list()) {
    stopifnot(typeof(samples) == "character", length(samples) > 0)
    if(typeof(numeric_filters) != "environment"){
      stop("numeric_filters must be of type environment (class: Cohort_Numeric_Filters)")
    }
    if(!all(class(numeric_filters) == c("Cohort_Numeric_Filters", "R6"))){
      stop("numeric_filters must be of class Cohort_Numeric_Filters")
    }
    purrr::map(catgegorical_filters, validate_catgegorical_filter)

    self$samples <- samples
    self$numeric_filters <- numeric_filters
    self$catgegorical_filters <- catgegorical_filters
  }
))

validate_catgegorical_filter <- function(catgegorical_filter){
  if(typeof(catgegorical_filter) != "list"){
    stop("Categorical Filter must be a list")
  }
  names_correct <- all(
    length(names(catgegorical_filter)) == 2,
    all(c("parent_group_choice", "group_choices") %in% (names(catgegorical_filter)))
  )
  if(!names_correct){
    stop("Categorical Filter must have names c('parent_group_choice', 'group_choices')")
  }
  if(length(catgegorical_filter$parent_group_choice) != 1){
    stop("Categorical Filter parent_group_choice must be length of 1")
  }
  if(length(catgegorical_filter$group_choices) == 0){
    stop("Categorical Filter group_choices must be length of atleast 1")
  }
  if(typeof(catgegorical_filter$parent_group_choice) != "character"){
    stop("Categorical Filter parent_group_choice must be character")
  }
  if(typeof(catgegorical_filter$group_choices) != "character"){
    stop("Categorical Filter group_choices must be character")
  }
}


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
      dplyr::pull(sample)
  }
))

validate_filter_name <- function(name){
  if(length(name) != 1){
    stop("Numeric Filter name must be length of 1")
  }
  if(typeof(name) != "character"){
    stop("Numeric Filter name must be character")
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

