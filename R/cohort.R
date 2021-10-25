
Cohort <- R6::R6Class(
  classname = "Cohort",
  public = list(
    dataset_names    = NULL,
    dataset_displays = NULL,
    group_type       = NULL,
    cohort_names     = NULL,
    group_tbl        = NULL,
    group_name       = NULL,
    group_display    = NULL,
    feature_tbl      = NULL,
    sample_tbl       = NULL,
    filters          = NULL,
    plot_colors      = NULL,
    initialize = function(
      filter_object,
      group_object
    ){

      if("TagGroup" %in% class(group_object)) group_type <- "tag"
      else if ("FeatureBinGroup" %in% class(group_object)) group_type <- "feature_bin"
      else if ("MutationStatusGroup" %in% class(group_object)) group_type <- "mutation_status"
      else stop("Unrecognized Group Class")

      dataset_names  <- group_object$dataset_names
      cohort_names   <- group_object$cohort_names
      group_name     <- group_object$group_name
      group_display  <- group_object$group_display


      sample_tbl <- filter_object$get_sample_tbl(cohort_names)
      tables     <- group_object$get_tables(sample_tbl)
      sample_tbl <- tables$sample_tbl
      group_tbl  <- tables$group_tbl



      plot_colors <- group_tbl %>%
        dplyr::select("short_name", "color") %>%
        tibble::deframe(.)

      dataset_displays <-
        iatlas.api.client::query_datasets() %>%
        dplyr::filter(.data$name %in% dataset_names) %>%
        dplyr::pull("display")

      feature_tbl <- iatlas.api.client::query_features(cohorts = cohort_names)

      # set values ----

      self$sample_tbl       <- sample_tbl
      self$group_tbl        <- group_tbl
      self$group_name       <- group_name
      self$group_display    <- group_display
      self$cohort_names     <- cohort_names
      self$dataset_names    <- dataset_names
      self$dataset_displays <- dataset_displays
      self$group_type       <- group_type
      self$feature_tbl      <- feature_tbl
      self$sample_tbl       <- sample_tbl
      self$filters          <- filter_object
      self$plot_colors      <- plot_colors

    },
    get_feature_class_list = function(){
      self$feature_tbl %>%
        dplyr::pull("class") %>%
        unique() %>%
        sort()
    },
    get_feature_list = function(){
      self$feature_tbl %>%
        dplyr::pull("name") %>%
        unique() %>%
        sort()
    },
    has_classes = function(classes, all_classes = T){
      cohort_classes <- self$get_feature_class_list()
      if(all_classes) has_function <- base::all
      else has_function <- base::any
      has_function(classes %in% cohort_classes)
    },
    has_features = function(features, all_features = T){
      cohort_features <- self$get_feature_list()
      if(all_features) has_function <- base::all
      else has_function <- base::any
      has_function(features %in% cohort_features)
    },

    get_feature_values = function(
      features = NA, feature_classes = NA, groups = NA
    ){
      tbl <- iatlas.api.client::query_feature_values(
        cohorts = self$cohort_names,
        features = features,
        feature_classes = feature_classes
      ) %>%
        dplyr::inner_join(self$sample_tbl, by = c("sample" = "sample_name")) %>%
        dplyr::rename("sample_name" = "sample") %>%
        dplyr::select(-"dataset_name") %>%
        dplyr::inner_join(self$group_tbl, by = c("group_name" = "short_name")) %>%
        dplyr::rename(
          "group_short_name" = "group_name",
          "group_long_name"  = "long_name",
          "group_characteristics" = "characteristics",
          "group_color" = "color",
          "group_order" = "order"
        ) %>%
        dplyr::select(-"size")


      if(!is.na(groups)){
        tbl <- dplyr::filter(tbl, .data$group_short_name %in% groups)
      }
      return(tbl)
    },

    get_gene_values = function(entrez = NA, gene_types = NA){
      iatlas.api.client::query_gene_expression(
        cohorts = self$cohort_names,
        gene_types = gene_types,
        entrez = entrez
      ) %>%
        dplyr::inner_join(self$sample_tbl, by = c("sample" = "sample_name")) %>%
        dplyr::rename("sample_name" = "sample") %>%
        dplyr::select(-"dataset_name") %>%
        dplyr::inner_join(self$group_tbl, by = c("group_name" = "short_name")) %>%
        dplyr::rename(
          "group_short_name" = "group_name",
          "group_long_name"  = "long_name",
          "group_characteristics" = "characteristics",
          "group_color" = "color",
          "group_order" = "order"
        ) %>%
        dplyr::select(-"size")
    }
  )
)
