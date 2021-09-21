TagGroup <- R6::R6Class(
  classname = "TagGroup",
  public = list(
    dataset_name = NULL,
    cohort = NULL,
    group_name = NULL,
    group_display = NULL,
    initialize = function(dataset_name, group_name){

      if(typeof(dataset_name) != "character"){
        stop("dataset_name must be of type character")
      }
      if(typeof(group_name) != "character"){
        stop("dataset_name must be of type character")
      }

      group_display <- group_name %>%
        iatlas.api.client::query_tags(tags = .) %>%
        dplyr::pull("tag_short_display")

      if(length(group_display) == 0) {
        stop("group_name not found in database")
      }

      cohort <-
        iatlas.api.client::query_cohorts(
          datasets = dataset_name,
          tags = group_name
        ) %>%
        dplyr::pull("name")

      if(length(group_display) == 0) {
        stop("cohort not found in database")
      }

      self$dataset_name <- dataset_name
      self$cohort <- cohort
      self$group_name <- group_name
      self$group_display <- group_display
    },
    get_tables = function(sample_tbl){

      sample_tbl <- sample_tbl %>%
        dplyr::select(
          "name" = "tag_long_display",
          "group" = "tag_short_display",
          "characteristics" = "tag_characteristics",
          "color" = "tag_color",
          "sample" = "sample_name"
        )

      group_tbl <- sample_tbl %>%
        dplyr::group_by(dplyr::across(c(-"sample"))) %>%
        dplyr::count(name = "size") %>%
        dplyr::ungroup() %>%
        dplyr::arrange(.data$group) %>%
        add_plot_colors_to_tbl(.) %>%
        dplyr::select("name", "group", "characteristics", "color", "size")

      sample_tbl <- sample_tbl %>%
        dplyr::select("sample", "group") %>%
        dplyr::arrange(.data$sample)

      return(list("sample_tbl" = sample_tbl, "group_tbl" = group_tbl))
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

      feature_display <-
        iatlas.api.client::query_features(features = feature_name) %>%
        dplyr::pull("display")

      group_name  = stringr::str_c("Immune Feature Bins: ", feature_display)

      self$dataset_name  <- dataset_name
      self$cohort        <- dataset_name
      self$group_name    <- group_name
      self$group_display <- group_name
      self$feature_name  <- feature_name
      self$feature_bins  <- feature_bins
    },
    get_tables = function(sample_tbl){
      sample_tbl <- iatlas.api.client::query_feature_values(
        features = self$feature_name, cohorts = self$dataset_name
      ) %>%
        dplyr::mutate("group" = as.character(cut(.data$feature_value, self$feature_bins))) %>%
        dplyr::select("sample", "group")

      group_tbl <- sample_tbl %>%
        dplyr::group_by(.data$group) %>%
        dplyr::summarise(size = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          "name" = self$feature_name,
          "characteristics" = "Immune feature bin range"
        ) %>%
        dplyr::arrange(.data$group) %>%
        add_plot_colors_to_tbl()

      return(list("sample_tbl" = sample_tbl, "group_tbl" = group_tbl))
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

      group_name <- stringr::str_c("Mutation Status: ", mutation_name)

      self$dataset_name  <- dataset_name
      self$cohort        <- dataset_name
      self$group_name    <- group_name
      self$group_display <- group_name
      self$mutation_name <- mutation_name
    },
    get_tables = function(sample_tbl){
      sample_tbl <-
        iatlas.api.client::query_mutation_statuses(
          mutations = self$mutation_name,
          cohorts = self$cohort
        ) %>%
        dplyr::select("sample" = "sample_name", "group" = "mutation_status")

      group_tbl <- sample_tbl %>%
        dplyr::group_by(.data$group) %>%
        dplyr::summarise(size = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(
          "name" = self$mutation_name,
          "characteristics" = "Mutation Status"
        ) %>%
        dplyr::arrange(.data$group) %>%
        add_plot_colors_to_tbl()

      return(list("sample_tbl" = sample_tbl, "group_tbl" = group_tbl))
    }
  )
)

