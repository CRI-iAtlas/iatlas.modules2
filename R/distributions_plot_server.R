distributions_plot_server <- function(
  id,
  plot_data_function,
  features = shiny::reactive(NULL),
  drilldown = shiny::reactive(F)
  ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      feature_classes <- shiny::reactive({
        if(is.null(features())){
          return(character(0))
        } else {
          features() %>%
            colnames() %>%
            setdiff(c("feature_name", "feature_display")) %>%
            return()
        }
      })

      display_feature_class_selection_ui <- shiny::reactive({
        shiny::req(!is.null(feature_classes()))
        length(feature_classes()) > 1
      })

      output$display_feature_class_selection_ui <- shiny::reactive({
        display_feature_class_selection_ui()
      })

      shiny::outputOptions(
        output,
        "display_feature_class_selection_ui",
        suspendWhenHidden = FALSE
      )

      output$feature_class_selection_ui <- shiny::renderUI({
        shiny::req(feature_classes())
        shiny::selectInput(
          inputId  = ns("feature_class_choice"),
          label    = "Select Feature",
          choices  = feature_classes()
        )
      })

      display_feature_selection_ui <- shiny::reactive({
        !is.null(features())
      })

      output$display_feature_selection_ui <- shiny::reactive({
        display_feature_selection_ui()
      })

      shiny::outputOptions(
        output,
        "display_feature_selection_ui",
        suspendWhenHidden = FALSE
      )

      feature_list <- shiny::reactive({
        shiny::req(
          display_feature_selection_ui(),
          !is.null(display_feature_class_selection_ui())
        )
        if(display_feature_class_selection_ui()){
          shiny::req(input$feature_class_choice)
          tbl <- features() %>%
            dplyr::select(
              "feature_class" = input$feature_class_choice,
              "feature_display",
              "feature_name"
            )
        } else {
          tbl <- features() %>%
            dplyr::select(
              "feature_class",
              "feature_display",
              "feature_name"
            )
        }
        create_nested_named_list(tbl)
      })

      output$feature_selection_ui <- shiny::renderUI({
        shiny::req(feature_list())
        shiny::selectInput(
          inputId  = ns("feature_choice"),
          label    = "Select Feature",
          choices  = feature_list()
        )
      })

      distplot_data <- shiny::reactive({
        shiny::req(
          plot_data_function(),
          input$scale_method_choice,
          input$reorder_method_choice
        )

        if(display_feature_selection_ui()){
          shiny::req(input$feature_choice)
        }

        data <-
          plot_data_function()(.feature = input$feature_choice) %>%
          scale_tbl_value_column(input$scale_method_choice) %>%
          reafctor_by_tbl_value_column(input$reorder_method_choice) %>%
          dplyr::select(dplyr::any_of(
            c("sample", "feature", "feature_value", "group", "group_description")
          ))
      })

      distplot_source_name <- shiny::reactive(ns("distplot"))

      plotly_function <- shiny::reactive({
        if(input$plot_type_choice == "Violin") return(plotly_violin)
        else return(plotly_box)
      })

      output$distplot <- plotly::renderPlotly({
        shiny::req(distplot_data(), distplot_source_name(), plotly_function())
        plotly_function()(
          data = distplot_data(),
          source_name = distplot_source_name(),
          x_col = "group",
          y_col = "feature_value"
        )
      })

      distplot_eventdata <- shiny::reactive({
        shiny::req(distplot_source_name(), distplot_data(), plotly_function())
        eventdata <- plotly::event_data("plotly_click", distplot_source_name())
        shiny::validate(shiny::need(eventdata, "Click on above barplot."))
        return(eventdata)
      })

      group_data <- shiny::reactive({
        shiny::req("group_description" %in% colnames(distplot_data()))
        distplot_data() %>%
          dplyr::select("group", "description" = "group_description") %>%
          dplyr::distinct()
      })

      plotly_server(
        "distplot",
        plot_data = distplot_data,
        group_data = group_data,
        eventdata = distplot_eventdata
      )

      drilldown_histogram_server(
        "histogram",
        plot_data = distplot_data,
        eventdata = distplot_eventdata,
        x_lab = "test_feature"
      )

      output$display_drilldown_ui <- shiny::reactive({
        drilldown()
      })

      shiny::outputOptions(
        output,
        "display_drilldown_ui",
        suspendWhenHidden = FALSE
      )
    }
  )
}
