cohort_manual_selection_server <- function(
  id,
  default_datasets = shiny::reactive("TCGA"),
  default_group    = shiny::reactive("Immune_Subtype"),
  dataset_type     = shiny::reactive("analysis")
){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      selected_datasets <- cohort_dataset_selection_server(
        "dataset_selection",
        default_datasets,
        dataset_type
      )

      datasets <- dedupe(shiny::reactive({
        shiny::req(default_datasets())
        if (is.null(selected_datasets())) return(default_datasets())
        else return(selected_datasets())
      }))

      dataset_feature_tbl <- shiny::reactive({
        shiny::req(datasets())
        iatlas.api.client::query_features(cohorts = datasets())
      })

      group_object <- cohort_group_selection_server(
        "group_selection",
        features_tbl = dataset_feature_tbl,
        selected_datasets = datasets,
        default_group = default_group
      )

      filter_object <- cohort_filter_selection_server(
        "filter_selection",
        datasets,
        dataset_feature_tbl
      )

      cohort_object <- shiny::reactive({
        shiny::req(group_object(), filter_object())
        Cohort$new(
          filter_object = filter_object(),
          group_object = group_object()
        )
      })

      return(cohort_object)
    }
  )
}
