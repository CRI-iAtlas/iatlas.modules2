
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

      # sample_tbl <- iatlas.api.client::query_cohort_samples(cohorts = cohort)
      # samples <- filter_object$get_samples(cohort, sample_tbl$sample_name)

      sample_tbl <- filter_object$get_sample_tbl(cohort)

      # tag_types ----
      if(group_type == "tag"){

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

      # mutation_types ----
      } else if(group_type == "mutation_status"){

        mutation_name <- group_object$mutation_name


        sample_tbl <-
          iatlas.api.client::query_mutation_statuses(
            mutations = mutation_name,
            cohorts = cohort
          ) %>%
          dplyr::select("sample" = "sample_name", "group" = "mutation_status")

        group_tbl <- sample_tbl %>%
          dplyr::group_by(.data$group) %>%
          dplyr::summarise(size = dplyr::n(), .groups = "drop") %>%
          dplyr::mutate(
            "name" = mutation_name,
            "characteristics" = "Mutation Status"
          ) %>%
          dplyr::arrange(.data$group) %>%
          add_plot_colors_to_tbl()

        group_name <- stringr::str_c("Mutation Status: ", mutation_name)

      # feature bins ----
      } else if (group_type == "feature_bin"){

        feature_name <- group_object$feature_name
        feature_bins <- group_object$feature_bins

        feature_display <- feature_tbl %>%
          dplyr::filter(.data$name == feature_name) %>%
          dplyr::pull("display")

        sample_tbl <- iatlas.api.client::query_feature_values(
          features = feature_name, cohorts = dataset_name
        ) %>%
          dplyr::mutate("group" = as.character(cut(.data$feature_value, feature_bins))) %>%
          dplyr::select("sample", "group")

        group_tbl <- sample_tbl %>%
          dplyr::group_by(.data$group) %>%
          dplyr::summarise(size = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            "name" = feature_name,
            "characteristics" = "Immune feature bin range"
          ) %>%
          dplyr::arrange(.data$group) %>%
          add_plot_colors_to_tbl()

        group_name  = stringr::str_c("Immune Feature Bins:", feature_display)

      }

      # other ----

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
