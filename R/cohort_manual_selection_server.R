cohort_manual_selection_server <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      default_dataset <- "PCAWG"

      selected_dataset <- cohort_dataset_selection_server(
        "dataset_selection",
        default_dataset
      )
      dataset <- dedupe(shiny::reactive({
        shiny::req(default_dataset)
        if (is.null(selected_dataset())) return(default_dataset)
        else return(selected_dataset())
      }))

      # TODO: fix using dataset cohort
      samples_tbl <- shiny::reactive({
        if(dataset() == "TCGA"){
          cohort <- "TCGA_TCGA_Study"
        } else {
          cohort <- "PCAWG_PCAWG_Study"
        }
        shiny::req(dataset())
        iatlas.api.client::query_cohort_samples(cohorts = cohort)
      })

      features_tbl <- shiny::reactive({
        shiny::req(dataset())
        iatlas.api.client::query_features(cohorts = dataset())
      })

      group_object <- cohort_group_selection_server(
        "group_selection",
        dataset
      )

      filter_object <- cohort_filter_selection_server(
        "filter_selection",
        dataset,
        samples_tbl,
        features_tbl
      )

      cohort_object <- shiny::reactive({
        shiny::req(group_object(), filter_object())
        build_cohort_object_from_objects(group_object(), filter_object())
      })
      return(cohort_object)
    }
  )
}
