
Cohort <- R6::R6Class(
  classname = "Cohort",
  public = list(
    dataset_name = NULL,
    dataset_display = NULL,
    group_type = NULL,
    cohort = NULL,
    group_tbl = NULL,
    group_name = NULL,
    group_display = NULL,
    feature_tbl = NULL,
    sample_tbl = NULL,
    filters = NULL,
    plot_colors = NULL,
    initialize = function(
      feature_tbl,
      filter_object,
      group_object
    ){

      if("TagGroup" %in% class(group_object)) group_type <- "tag"
      else if ("FeatureBinGroup" %in% class(group_object)) group_type <- "feature_bin"
      else if ("MutationStatusGroup" %in% class(group_object)) group_type <- "mutation_status"
      else stop("Unrecognized Group Class")

      dataset_name  <- group_object$dataset_name
      cohort        <- group_object$cohort
      group_name    <- group_object$group_name
      group_display <- group_object$group_display

      sample_tbl <- filter_object$get_sample_tbl(cohort)
      tables     <- group_object$get_tables(sample_tbl)
      sample_tbl <- tables$sample_tbl
      group_tbl  <- tables$group_tbl

      plot_colors <- group_tbl %>%
        dplyr::select("group", "color") %>%
        tibble::deframe(.)

      dataset_display <-
        iatlas.api.client::query_datasets() %>%
        dplyr::filter(.data$name == dataset_name) %>%
        dplyr::pull("display")

      # set values ----

      self$cohort        <- cohort
      self$sample_tbl    <- sample_tbl
      self$group_tbl     <- group_tbl
      self$group_name    <- group_name
      self$group_display <- group_display

      self$dataset_name    <- dataset_name
      self$dataset_display <- dataset_display
      self$group_type      <- group_type
      self$feature_tbl     <- feature_tbl
      self$sample_tbl      <- sample_tbl
      self$filters         <- filter_object
      self$plot_colors     <- plot_colors

    }
  )
)
