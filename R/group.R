TagGroup <- R6::R6Class(
  classname = "TagGroup",
  public = list(
    dataset_names = NULL,
    cohort_names = NULL,
    group_name = NULL,
    group_display = NULL,
    initialize = function(dataset_names, group_name){

      if(typeof(dataset_names) != "character"){
        stop("dataset_names must be of type character")
      }
      if(length(dataset_names) == 0){
        stop("dataset_names must be of length atleast 1")
      }
      if(typeof(group_name) != "character"){
        stop("group_name must be of type character")
      }
      if(length(group_name) != 1){
        stop("group_names= must be of length 1")
      }

      group_display <- group_name %>%
        iatlas.api.client::query_tags(tags = .) %>%
        dplyr::pull("tag_short_display")

      if(length(group_display) == 0) {
        stop("group_name not found in database")
      }

      cohort_names <-
        iatlas.api.client::query_cohorts(
          datasets = dataset_names,
          tags = group_name
        ) %>%
        dplyr::pull("name")

      if(length(group_display) == 0) {
        stop("cohort not found in database")
      }

      self$dataset_names <- dataset_names
      self$cohort_names  <- cohort_names
      self$group_name    <- group_name
      self$group_display <- group_display
    },
    get_tables = function(sample_tbl){

      sample_tbl <- sample_tbl %>%
        dplyr::select(
          "short_name" = "tag_short_display",
          "long_name" = "tag_long_display",
          "characteristics" = "tag_characteristics",
          "color" = "tag_color",
          "order" = "tag_order",
          "sample_name",
          "dataset_name",
          "dataset_display"
        )

      group_tbl <- sample_tbl %>%
        dplyr::group_by(dplyr::across(c(-"sample_name"))) %>%
        dplyr::count(name = "size") %>%
        dplyr::ungroup() %>%
        dplyr::arrange(.data$short_name) %>%
        add_plot_colors_to_tbl(.) %>%
        dplyr::select(
          "short_name",
          "long_name",
          "characteristics",
          "color",
          "size",
          "order",
          "dataset_name",
          "dataset_display"
        )

      sample_tbl <- sample_tbl %>%
        dplyr::select("sample_name", "group_name" = "short_name", "dataset_name") %>%
        dplyr::arrange(.data$sample_name)

      return(list("sample_tbl" = sample_tbl, "group_tbl" = group_tbl))
    }

  )
)

FeatureBinGroup <- R6::R6Class(
  classname = "FeatureBinGroup",
  public = list(
    dataset_names = NULL,
    cohort_names  = NULL,
    group_name    = NULL,
    group_display = NULL,
    feature_name  = NULL,
    feature_bins  = NULL,
    initialize = function(dataset_names, feature_name, feature_bins){

      feature_display <-
        iatlas.api.client::query_features(features = feature_name) %>%
        dplyr::pull("display")

      group_name  = stringr::str_c("Immune Feature Bins: ", feature_display)

      self$dataset_names  <- dataset_names
      self$cohort_names   <- dataset_names
      self$group_name     <- group_name
      self$group_display  <- group_name
      self$feature_name   <- feature_name
      self$feature_bins   <- feature_bins
    },
    get_tables = function(sample_tbl){

      sample_tbl <- iatlas.api.client::query_feature_values(
        features = self$feature_name, cohorts = self$dataset_names
      ) %>%
        dplyr::mutate("group_name" = as.character(cut(.data$feature_value, self$feature_bins))) %>%
        dplyr::inner_join(sample_tbl, by = c("sample" = "sample_name")) %>%
        dplyr::select("sample_name" = "sample", "group_name", "dataset_name", "dataset_display")


      group_tbl <- sample_tbl %>%
        dplyr::group_by(dplyr::across(c(-"sample_name"))) %>%
        dplyr::summarise(size = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(
          "short_name" = self$group_name,
          "long_name" = self$feature_name,
          "characteristics" = "Immune feature bin range",
          "order" = NA
        ) %>%
        dplyr::arrange(.data$group_name) %>%
        add_plot_colors_to_tbl() %>%
        dplyr::select(
          "short_name",
          "long_name",
          "characteristics",
          "color",
          "size",
          "order",
          "dataset_name",
          "dataset_display"
        )

      sample_tbl <- dplyr::select(sample_tbl, "sample_name", "group_name", "dataset_name")

      return(list("sample_tbl" = sample_tbl, "group_tbl" = group_tbl))
    }
  )
)

MutationStatusGroup <- R6::R6Class(
  classname = "MutationStatusGroup",
  public = list(
    dataset_names = NULL,
    cohort_names  = NULL,
    group_name    = NULL,
    group_display = NULL,
    mutation_name = NULL,
    initialize = function(dataset_names, mutation_name){

      group_name <- stringr::str_c("Mutation Status: ", mutation_name)

      self$dataset_names <- dataset_names
      self$cohort_names  <- dataset_names
      self$group_name    <- group_name
      self$group_display <- group_name
      self$mutation_name <- mutation_name
    },
    get_tables = function(sample_tbl){

      sample_tbl <-
        iatlas.api.client::query_mutation_statuses(
          mutations = self$mutation_name,
          cohorts = self$cohort_names
        ) %>%
        dplyr::select("sample_name", "group_name" = "mutation_status") %>%
        dplyr::inner_join(sample_tbl, by = "sample_name") %>%
        dplyr::select("sample_name", "group_name", "dataset_name", "dataset_display")

      group_tbl <- sample_tbl %>%
        dplyr::group_by(dplyr::across(c(-"sample_name"))) %>%
        dplyr::summarise(size = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(
          "short_name" = self$group_name,
          "long_name" = self$group_name,
          "characteristics" = "Mutation Status",
          "order" = NA
        ) %>%
        dplyr::arrange(.data$group_name) %>%
        add_plot_colors_to_tbl() %>%
        dplyr::select(
          "short_name",
          "long_name",
          "characteristics",
          "color",
          "size",
          "order",
          "dataset_name",
          "dataset_display"
        )

      sample_tbl <- dplyr::select(sample_tbl, "sample_name", "group_name", "dataset_name")

      return(list("sample_tbl" = sample_tbl, "group_tbl" = group_tbl))
    }
  )
)

