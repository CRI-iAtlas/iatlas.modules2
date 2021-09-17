TagGroup <- R6::R6Class(
  classname = "TagGroup",
  public = list(
    dataset_name = NULL,
    cohort = NULL,
    group_name = NULL,
    group_display = NULL,
    initialize = function(dataset_name, group_name){
      group_display <- group_name %>%
        iatlas.api.client::query_tags(tags = .) %>%
        dplyr::pull("tag_short_display")

      cohort <-
        iatlas.api.client::query_cohorts(
          datasets = dataset_name,
          tags = group_name
        ) %>%
        dplyr::pull("name")

      self$dataset_name <- dataset_name
      self$cohort <- cohort
      self$group_name <- group_name
      self$group_display <- group_display
    }
  )
)

FeatureBinGroup <- R6::R6Class(
  classname = "FeatureBinGroup",
  public = list(
    dataset_name = NULL,
    cohort = NULL,
    group_name = NULL,
    group_display = NULL,
    feature_name = NULL,
    feature_bins = NULL,
    initialize = function(dataset_name, feature_name, feature_bins){
      self$dataset_name <- dataset_name
      self$cohort <- dataset_name
      self$group_name <- "Immune Feature Bins"
      self$group_display <- "Immune Feature Bins"
      self$feature_name <- feature_name
      self$feature_bins <- feature_bins
    }
  )
)

MutationStatusGroup <- R6::R6Class(
  classname = "MutationStatusGroup",
  public = list(
    dataset_name = NULL,
    cohort = NULL,
    group_name = NULL,
    group_display = NULL,
    mutation_name = NULL,
    initialize = function(dataset_name, mutation_name){
      self$dataset_name <- dataset_name
      self$cohort <- dataset_name
      self$group_name <- "Driver Mutation"
      self$group_display <- "Driver Mutation"
      self$mutation_name <- mutation_name
    }
  )
)

