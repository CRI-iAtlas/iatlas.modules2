cohort_manual_selection_server <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      default_dataset <- "TCGA"
      default_group   <- "Immune_Subtype"

      selected_dataset <- cohort_dataset_selection_server(
        "dataset_selection",
        default_dataset
      )

      dataset <- dedupe(shiny::reactive({
        shiny::req(default_dataset)
        if (is.null(selected_dataset())) return(default_dataset)
        else return(selected_dataset())
      }))

      dataset_feature_tbl <- shiny::reactive({
        shiny::req(dataset())
        iatlas.api.client::query_features(cohorts = dataset())
      })

      group_object <- cohort_group_selection_server(
        "group_selection",
        dataset,
        default_group,
        dataset_feature_tbl
      )

      filter_object <- cohort_filter_selection_server(
        "filter_selection",
        dataset,
        dataset_feature_tbl
      )

      sample_tbl <- shiny::reactive({
        shiny::req(group_object())
        iatlas.api.client::query_cohort_samples(cohorts = group_object()$cohort)
      })



      cohort_object <- shiny::reactive({
        shiny::req(group_object(), filter_object(), sample_tbl, dataset_feature_tbl())
        Cohort$new(
          feature_tbl = dataset_feature_tbl(),
          sample_tbl = sample_tbl(),
          filter_object = filter_object(),
          group_object = group_object()
        )
      })

      return(cohort_object)
    }
  )
}
