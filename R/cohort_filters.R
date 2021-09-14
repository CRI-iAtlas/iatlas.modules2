Cohort_Filters <- R6::R6Class("Cohort_Filters", list(
  samples = NULL,
  numeric_filters = NULL,
  catgegorical_filters = NULL,
  initialize = function(samples, numeric_filters = list(), catgegorical_filters = list()) {
    stopifnot(typeof(samples) == "character", length(samples) > 0)
    purrr::map(numeric_filters, validate_numeric_filter)
    purrr::map(catgegorical_filters, validate_catgegorical_filter)

    self$samples <- samples
    self$numeric_filters <- numeric_filters
    self$catgegorical_filters <- catgegorical_filters
  }
))

validate_numeric_filter <- function(numeric_filter){
  if(typeof(numeric_filter) != "list"){
    stop("Numeric Filter must be a list")
  }
  names_correct <- all(
    length(names(numeric_filter)) == 3,
    all(c("name", "min", "max") %in% (names(numeric_filter)))
  )
  if(!names_correct){
    stop("Numeric Filter must have names c('name', 'min', 'max')")
  }
  if(length(numeric_filter$name) != 1){
    stop("Numeric Filter name must be length of 1")
  }
  if(length(numeric_filter$min) != 1){
    stop("Numeric Filter min must be length of 1")
  }
  if(length(numeric_filter$max) != 1){
    stop("Numeric Filter max must be length of 1")
  }
  if(typeof(numeric_filter$name) != "character"){
    stop("Numeric Filter name must be character")
  }
  if(!typeof(numeric_filter$min) %in% c("numeric", "integer", "double")){
    stop("Numeric Filter min must be numeric")
  }
  if(!typeof(numeric_filter$max) %in% c("numeric", "integer", "double")){
    stop("Numeric Filter min must be numeric")
  }
}

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
