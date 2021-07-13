cohort_filter_selection_server <- function(
  id,
  dataset,
  samples_tbl,
  features_tbl
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      samples <- shiny::reactive({
        shiny::req(samples_tbl())
        dplyr::pull(samples_tbl(),"sample_name")
      })


      # group filters -----------------------------------------------------------
      tag_group_filter_tbl <- shiny::reactive({
        shiny::req(dataset())
        iatlas.api.client::query_dataset_tags(dataset = dataset()) %>%
          dplyr::select("display" = "tag_short_display", "name" = "tag_name")
      })

      group_filter_list <- shiny::reactive({
        shiny::req(tag_group_filter_tbl())
        lst <-
          dplyr::bind_rows(
            tag_group_filter_tbl()
          ) %>%
          dplyr::select("display", "name") %>%
          tibble::deframe(.)
      })

      group_element_module_server <- shiny::reactive({
        shiny::req(group_filter_list())
        purrr::partial(
          group_filter_element_server,
          group_named_list = group_filter_list,
          dataset = dataset
        )
      })

      group_element_module_ui <- shiny::reactive(group_filter_element_ui)

      group_filter_output <- insert_remove_element_server(
        "group_filter",
        element_module = group_element_module_server,
        element_module_ui = group_element_module_ui,
        remove_ui_event = shiny::reactive(dataset())
      )

      valid_group_filter_obj <- shiny::reactive({
        shiny::req(group_filter_output())
        group_filter_output() %>%
          shiny::reactiveValuesToList(.) %>%
          get_valid_group_filters()
      })

      group_filter_samples <- shiny::reactive({
        shiny::req(samples, dataset())
        get_group_filtered_samples(
          valid_group_filter_obj(),
          samples(),
          dataset()
        )
      })

      # # numeric_filters -------------------------------------------------------

      feature_tbl <- shiny::reactive({
        shiny::req(dataset(), features_tbl())
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
          dataset = dataset
        )
      })

      numeric_element_module_ui <- shiny::reactive(numeric_filter_element_ui)

      numeric_filter_output <- insert_remove_element_server(
        "numeric_filter",
        element_module = numeric_element_module_server,
        element_module_ui = numeric_element_module_ui,
        remove_ui_event = shiny::reactive(dataset())
      )

      valid_numeric_filter_obj <- shiny::reactive({
        shiny::req(numeric_filter_output())
        numeric_filter_output() %>%
          shiny::reactiveValuesToList(.) %>%
          get_valid_numeric_filters()
      })

      numeric_filter_samples <- shiny::reactive({
        shiny::req(samples)
        get_numeric_filtered_samples(
          valid_numeric_filter_obj(),
          samples(),
          dataset()
        )
      })

      # return filter obj ----

      selected_samples <- shiny::reactive({
        shiny::req(numeric_filter_samples(), group_filter_samples())
        intersect(numeric_filter_samples(), group_filter_samples())
      })

      output$samples_text <- shiny::renderText({
        c("Number of current samples:", length(selected_samples()))
      })

      filter_obj <- shiny::reactive({
        list(
          "samples" = selected_samples(),
          "filters" = list(
            "numeric_filters" = valid_numeric_filter_obj(),
            "tag_filters" = valid_group_filter_obj()
          )
        )
      })

      return(filter_obj)
    }
  )
}
