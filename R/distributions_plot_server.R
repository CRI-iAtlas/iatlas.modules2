distributions_plot_server <- function(
  id,
  plot_data_function
  # plot_data
  # group_data    = shiny::reactive(NULL),
  # feature_data  = shiny::reactive(NULL)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$display_feature_class_selection_ui <- shiny::reactive({
        display_feature_class_selection_ui()
      })

      feature

      shiny::outputOptions(
        output,
        "display_feature_class_selection_ui",
        suspendWhenHidden = FALSE
      )

      output$feature_class_selection_ui <- shiny::renderUI({
        shiny::req(feature_classes())
        shiny::selectInput(
          inputId  = ns("feature_class_choice"),
          label    = "Select Feature Class",
          choices  = feature_classes()
        )
      })

      distplot_data <- shiny::reactive({
        shiny::req(plot_data_function())
        if(display_feature_class_selection_ui()){
          shiny::req(input$feature_class_choice)
        }
        data <-
          plot_data_function(.feature_class = feature_class_choice) %>%
          dplyr::select(dplyr::any_of(
            c("sample", "feature", "feature_value", "group", "group_description")
          ))
      })
        # shiny::req(plot_data())
        #
        # plot_data() %>%
        #   dplyr::filter(.data$feature == input$feature_choice) %>%
        #   dplyr::select("sample", "x", "y")
      })

      distplot_source_name <- shiny::reactive(ns("distplot"))

      output$distplot <- plotly::renderPlotly({
        shiny::req(distplot_data(), distplot_source_name())

        plotly_violin(
          data = distplot_data(),
          source_name = distplot_source_name()
        )
      })

      distplot_eventdata <- shiny::reactive({
        eventdata <- plotly::event_data("plotly_click", distplot_source_name())
        shiny::validate(shiny::need(eventdata, "Click on above plot."))
        return(eventdata)
      })


    }
  )
}
