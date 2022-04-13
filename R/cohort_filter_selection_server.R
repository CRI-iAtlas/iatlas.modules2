cohort_filter_selection_server <- function(
  id,
  datasets,
  features_tbl
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {


      # group filters -----------------------------------------------------------
      group_filter_list <- shiny::reactive({
        shiny::req(datasets())
        iatlasGraphQLClient::query_dataset_tags(dataset = datasets()) %>%
          dplyr::group_by(.data$tag_name) %>%
          dplyr::mutate(count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::filter(.data$count == length(datasets())) %>%
          dplyr::select("tag_short_display", "tag_name") %>%
          dplyr::arrange(.data$tag_name) %>%
          tibble::deframe(.)
      })

      group_element_module_server <- shiny::reactive({
        shiny::req(group_filter_list())
        purrr::partial(
          group_filter_element_server,
          group_named_list = group_filter_list,
          datasets = datasets
        )
      })

      group_element_module_ui <- shiny::reactive(group_filter_element_ui)

      group_filter_output <- insert_remove_element_server(
        "group_filter",
        element_module = group_element_module_server,
        element_module_ui = group_element_module_ui,
        remove_ui_event = shiny::reactive(datasets())
      )

      cohort_group_filter_obj <- shiny::reactive({
        shiny::req(group_filter_output())
        group_filter_output() %>%
          shiny::reactiveValuesToList(.) %>%
          purrr::discard(purrr::map_lgl(., is.null)) %>%
          unname() %>%
          CohortFilterList$new(type = "group")
      })

      # # numeric_filters -------------------------------------------------------

      feature_tbl <- shiny::reactive({
        shiny::req(features_tbl())
        features_tbl() %>%
          dplyr::select("class", "display", "feature" = "name")
      })

      numeric_named_list <- shiny::reactive({
        shiny::req(feature_tbl())
        lst <-
          dplyr::bind_rows(feature_tbl()) %>%
          iatlas.modules::create_nested_named_list(
            names_col1 = "class",
            names_col2 = "display",
            values_col = "feature"
          )
      })

      numeric_element_module_server <- shiny::reactive({
        purrr::partial(
          numeric_filter_element_server,
          numeric_named_list = numeric_named_list,
          datasets = datasets
        )
      })

      numeric_element_module_ui <- shiny::reactive(numeric_filter_element_ui)

      numeric_filter_output <- insert_remove_element_server(
        "numeric_filter",
        element_module = numeric_element_module_server,
        element_module_ui = numeric_element_module_ui,
        remove_ui_event = shiny::reactive(datasets())
      )

      cohort_numeric_filter_obj <- shiny::reactive({
        shiny::req(numeric_filter_output())
        numeric_filter_output() %>%
          shiny::reactiveValuesToList(.) %>%
          purrr::discard(purrr::map_lgl(., is.null)) %>%
          unname() %>%
          CohortFilterList$new(type = "numeric")
      })

      filter_object <- shiny::reactive({
        shiny::req(cohort_group_filter_obj(), cohort_numeric_filter_obj())
        CohortFilters$new(
          numeric_filters = cohort_numeric_filter_obj(),
          group_filters = cohort_group_filter_obj()
        )
      })

      return(filter_object)
    }
  )
}
