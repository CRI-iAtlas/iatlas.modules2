
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
        cohort = self$cohort,
        features = features,
        feature_classes = feature_classes
      ) %>%
        dplyr::filter(.data$sample %in% self$sample_tbl$sample)

      if(!is.na(groups)){
        group_samples <- self$sample_tbl %>%
          dplyr::filter(.data$group %in% groups) %>%
          dplyr::pull("sample")
        tbl <- dplyr::filter(tbl, .data$sample %in% group_samples)
      }
      return(tbl)
    },

    get_gene_values = function(entrez = NA, gene_types = NA){
      iatlas.api.client::query_gene_expression(
        cohort = self$cohort,
        gene_types = gene_types,
        entrez = entrez
      ) %>%
        dplyr::filter(.data$sample %in% self$sample_tbl$sample)
    }
  )
)
