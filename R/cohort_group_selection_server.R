cohort_group_selection_server <- function(
  id,
  selected_dataset,
  default_group,
  features_tbl
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Select group type ----

      tag_group_tbl <- shiny::reactive({
        shiny::req(selected_dataset())
        selected_dataset() %>%
          iatlas.api.client::query_dataset_tags() %>%
          dplyr::select("display" = "short_display", "name")
      })

      custom_group_tbl <- shiny::reactive({
        shiny::req(selected_dataset())
        build_custom_group_tbl(selected_dataset())
      })

      available_groups_list <- shiny::reactive({
        shiny::req(tag_group_tbl(), custom_group_tbl())
        build_cohort_group_list(
          tag_group_tbl(),
          custom_group_tbl()
        )
      })

      output$select_group_ui <- shiny::renderUI({
        shiny::req(available_groups_list(), default_group)
        shiny::selectInput(
          inputId = ns("group_choice"),
          label = shiny::strong("Select or Search for Grouping Variable"),
          choices = available_groups_list(),
          selected = default_group
        )
      })

      group_choice <- dedupe(shiny::reactive({
        shiny::req(default_group)
        if (is.null(input$group_choice)) return(default_group)
        else return(input$group_choice)
      }))

      # Driver Mutations ----

      display_driver_mutation_ui <- shiny::reactive({
        shiny::req(group_choice())
        group_choice() == "Driver Mutation"
      })

      # This is so that the conditional panel can see the various shiny::reactives
      output$display_driver_mutation <- shiny::reactive({
        display_driver_mutation_ui()
      })

      shiny::outputOptions(
        output,
        "display_driver_mutation",
        suspendWhenHidden = FALSE
      )

      mutation_tbl <- shiny::reactive({
        build_cohort_mutation_tbl()
      })

      output$select_driver_mutation_group_ui <- shiny::renderUI({
        shiny::req(group_choice() == "Driver Mutation", mutation_tbl())
        shiny::selectInput(
          inputId  = ns("driver_mutation_choice_id"),
          label    = "Select or Search for Driver Mutation",
          choices  = mutation_tbl() %>%
            dplyr::select("mutation", "mutation_id") %>%
            tibble::deframe(.)
        )
      })


      # Immune feature bins ----

      display_immune_feature_bins_ui <- shiny::reactive({
        shiny::req(group_choice())
        group_choice() == "Immune Feature Bins"
      })

      output$display_immune_feature_bins <- shiny::reactive({
        display_immune_feature_bins_ui()
      })

      shiny::outputOptions(
        output,
        "display_immune_feature_bins",
        suspendWhenHidden = FALSE
      )

      feature_bin_list <- shiny::reactive({
        shiny::req(group_choice() == "Immune Feature Bins", features_tbl())
        features_tbl() %>%
          dplyr::select("class", "display", "name") %>%
          iatlas.modules::create_nested_named_list(
            names_col1 = "class",
            names_col2 = "display",
            values_col = "name"
          )
      })

      output$select_immune_feature_bins_group_ui <- shiny::renderUI({
        shiny::req(feature_bin_list())

        shiny::selectInput(
          inputId = ns("bin_immune_feature_choice"),
          label = "Select or Search for feature",
          choices = feature_bin_list()
        )
      })

      # Group Object ----

      group_object <- shiny::reactive({
        shiny::req(selected_dataset(), group_choice())

        group_object <- list(
          "dataset" = selected_dataset(),
          "group_name" = group_choice()
        )

        if (group_choice() == "Driver Mutation") {
          shiny::req(input$driver_mutation_choice_id)
          group_object$group_display <- "Driver Mutation"
          group_object$group_type <- "custom"
          group_object$mutation_id <- as.integer(
            input$driver_mutation_choice_id
          )
        } else if (group_choice() == "Immune Feature Bins") {
          shiny::req(
            input$bin_immune_feature_choice,
            input$bin_number_choice
          )
          group_object$group_display <- "Immune Feature Bins"
          group_object$group_type <- "custom"
          group_object$bin_immune_feature <- input$bin_immune_feature_choice
          group_object$bin_number <- input$bin_number_choice
        } else {
          group_object$group_display <- group_choice() %>%
            iatlas.api.client::query_tags(tags = .) %>%
            dplyr::pull("short_display")
          group_object$group_type <- "tag"
        }


        return(group_object)
      })

      return(group_object)
    }
  )
}
