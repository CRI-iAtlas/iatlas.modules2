cohort_manual_selection_server <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      default_dataset <- "TCGA"

      selected_dataset <- cohort_dataset_selection_server(
        "dataset_selection",
        default_dataset
      )

      dataset <- dedupe(shiny::reactive({
        shiny::req(default_dataset)
        if (is.null(selected_dataset())) return(default_dataset)
        else return(selected_dataset())
      }))

      feature_tbl <- shiny::reactive({
        shiny::req(dataset())
        iatlas.api.client::query_features(cohorts = dataset())
      })

      group_object <- cohort_group_selection_server(
        "group_selection",
        dataset,
        feature_tbl
      )

      sample_tbl <- shiny::reactive({
        shiny::req(dataset(), group_object())

        if(group_object()$group_type == "tag"){
          cohort <-
            iatlas.api.client::query_cohorts(
              datasets = dataset(),
              tags = group_object()$group_name
            ) %>%
            dplyr::pull("name")
        } else if (group_object()$group_type == "clinical"){
          cohort <-
            iatlas.api.client::query_cohorts(
              datasets = dataset(),
              clinical = group_object()$group_display
            ) %>%
            dplyr::pull("name")
        } else {
          cohort = dataset()
        }
        iatlas.api.client::query_cohort_samples(cohorts = cohort)
      })

      filter_object <- cohort_filter_selection_server(
        "filter_selection",
        dataset,
        sample_tbl,
        feature_tbl
      )

      cohort_object <- shiny::reactive({

        obj <- build_cohort_object_from_objects(
          group_object(),
          filter_object(),
          feature_tbl(),
          sample_tbl()
        )
        return(obj)
      })

      return(cohort_object)
    }
  )
}
