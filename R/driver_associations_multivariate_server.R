#' Multivariate Driver Server
#'
#' @param id A string, a module id
#' @param cohort_obj A named list
#'
#' @export
multivariate_driver_server <- function(id, cohort_obj) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$response_options <- shiny::renderUI({

        shiny::selectInput(
          inputId  = ns("response_choice"),
          label    = "Select or Search for Response Variable",
          selected = "leukocyte_fraction",
          choices  = iatlas.modules::create_nested_named_list(
            cohort_obj()$feature_tbl,
            names_col1 = "class",
            names_col2 = "display",
            values_col = "name"
          )
        )
      })

      numerical_covariate_tbl <- shiny::reactive({
        cohort_obj() %>%
          purrr::pluck("feature_tbl") %>%
          dplyr::select("class", "display", "feature" = "name")
      })

      categorical_covariate_tbl <- shiny::reactive({
        dplyr::tribble(
          ~class,     ~display,         ~feature,
          "Groups",   "Immune Subtype", "Immune_Subtype",
          "Groups",   "TCGA Study",     "TCGA_Study",
          "Groups",   "TCGA Subtype",   "TCGA_Subtype"
        )
      })

      response_variable_display <- shiny::reactive({
        shiny::req(input$response_choice)
        cohort_obj() %>%
          purrr::pluck("feature_tbl") %>%
          dplyr::filter(.data$name == input$response_choice) %>%
          dplyr::pull("display")
      })

      model_string_prefix <- shiny::reactive({
        shiny::req(response_variable_display())
        stringr::str_c(response_variable_display(), " ~ Mutation status")
      })

      covariates_obj <- model_selection_server(
        "module1",
        numerical_covariate_tbl,
        categorical_covariate_tbl,
        model_string_prefix
      )

      output$model_text <- shiny::renderText({
        covariates_obj()$display_string
      })

      covariate_tbl <- shiny::reactive({
        shiny::req(covariates_obj())
        build_md_covariate_tbl(cohort_obj(), covariates_obj())
      })

      response_tbl <- shiny::reactive({
        shiny::req(input$response_choice)
        build_md_response_tbl(cohort_obj(), input$response_choice)
      })

      status_tbl <- shiny::reactive({
        iatlasGraphQLClient::query_mutation_statuses(
          samples = cohort_obj()$sample_tbl$sample
        ) %>%
          dplyr::select(
            "mutation" = "mutation_name",
            "sample" = "sample_name",
            "status" = "mutation_status"
          )
      })

      combined_tbl <- shiny::reactive({
        shiny::req(
          response_tbl(),
          status_tbl(),
          input$group_mode
        )
        combine_md_tbls(
          response_tbl(),
          status_tbl(),
          cohort_obj()$sample_tbl,
          covariate_tbl(),
          input$group_mode
        )
      })

      labels <- shiny::reactive({
        shiny::req(combined_tbl(), input$min_mutants, input$min_wildtype)
        filter_md_labels(combined_tbl(), input$min_mutants, input$min_wildtype)
      })

      filtered_tbl <- shiny::reactive({
        shiny::req(combined_tbl(), labels())
        dplyr::filter(combined_tbl(), .data$label %in% labels())
      })

      pvalue_tbl <- shiny::reactive({
        shiny::req(filtered_tbl(), covariates_obj())
        build_md_pvalue_tbl(filtered_tbl(), covariates_obj()$formula_string)
      })

      effect_size_tbl <- shiny::reactive({
        shiny::req(filtered_tbl())
        build_md_effect_size_tbl(filtered_tbl())
      })

      volcano_plot_tbl <- shiny::eventReactive(input$calculate_button, {
        shiny::req(pvalue_tbl(), effect_size_tbl())
        dplyr::inner_join(pvalue_tbl(), effect_size_tbl(), by = "label")
      })

      total_associations <- shiny::reactive({
        n_mutations <-
          iatlasGraphQLClient::query_mutations(
            datasets = "TCGA", types = "driver_mutation"
          ) %>%
          nrow()

        if(input$group_mode == "By group"){
          n_groups <- cohort_obj() %>%
            purrr::pluck("group_tbl") %>%
            nrow()
          return(n_groups * n_mutations)
        } else {
          return(n_mutations)
        }
      })

      output$result_text <- shiny::renderText({

        p_tested <-
          volcano_plot_tbl() %>%
          nrow() %>%
          magrittr::divide_by(., total_associations()) %>%
          round(2) %>%
          as.character()

        stringr::str_c("Percentage of Tested Associations: ", p_tested)
      })

      output$volcano_plot <- plotly::renderPlotly({
        shiny::req(volcano_plot_tbl())

        shiny::validate(shiny::need(
          nrow(volcano_plot_tbl()) > 0,
          paste0(
            "Current parameters did not result in any linear regression",
            "results."
          )
        ))
        iatlas.modules::plotly_scatter(
          volcano_plot_tbl(),
          x_col     = "log10_fold_change",
          y_col     = "log10_p_value",
          xlab      = "Log10(Fold Change)",
          ylab      = "- Log10(P-value)",
          title     = "Immune Response Association With Driver Mutations",
          key_col   = "label",
          label_col = "label",
          source_name       = "multivariate_volcano_plot",
          horizontal_line   = T,
          horizontal_line_y = (-log10(0.05))
        )
      })

      iatlas.modules::plotly_server(
        id = "volcano_plot",
        plot_data = volcano_plot_tbl
      )

      selected_volcano_result <- shiny::reactive({
        shiny::req(volcano_plot_tbl())

        eventdata <- plotly::event_data(
          "plotly_click",
          source = "multivariate_volcano_plot"
        )

        # plot not clicked on yet
        shiny::validate(shiny::need(
          !is.null(eventdata),
          paste0(
            "Click a point on the above scatterplot to see a violin plot ",
            "for the comparison"
          )
        ))

        clicked_label <- iatlas.modules::get_values_from_eventdata(eventdata, "key")

        result <-  dplyr::filter(
          volcano_plot_tbl(),
          label == clicked_label
        )

        shiny::validate(shiny::need(
          nrow(result) == 1,
          paste0(
            "Click a point on the above scatterplot to see a violin plot ",
            "for the comparison"
          )
        ))
        return(result)
      })

      violin_tbl <- shiny::reactive({
        shiny::req(filtered_tbl(), selected_volcano_result(), input$group_mode)
        build_md_driver_violin_tbl(
          filtered_tbl(),
          selected_volcano_result()$label
        )
      })

      output$violin_plot <- plotly::renderPlotly({
        shiny::req(
          selected_volcano_result(),
          response_variable_display(),
          input$group_mode,
          violin_tbl()
        )

        shiny::validate(shiny::need(
          nrow(violin_tbl()) > 0,
          "Parameters have changed, press the calculate boutton."
        ))

        iatlas.modules::plotly_violin(
          violin_tbl(),
          xlab = stringr::str_c(
            selected_volcano_result()$label, " Mutation Status"
          ),
          ylab = response_variable_display(),
          title = create_md_violin_plot_title(
            selected_volcano_result(), input$group_mode
          ),
          fill_colors = c("blue"),
          showlegend = FALSE
        )
      })

      iatlas.modules::plotly_server(
        id = "violin_plot",
        plot_data = violin_tbl
      )
    }
  )
}

