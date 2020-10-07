distributions_plot_server <- function(
  id,
  plot_data,
  group_data    = shiny::reactive(NULL),
  feature_data  = shiny::reactive(NULL)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      feature_choice_list <- shiny::reactive({
        shiny::req(feature_data())
        create_nested_named_list(feature_data())
      })

      output$feature_selection_ui <- shiny::renderUI({
        shiny::req(feature_choice_list())
        shiny::selectInput(
          inputId  = ns("feature_choice"),
          label    = "Select Feature",
          choices  = feature_choice_list()
        )
      })

      distplot_data <- shiny::reactive({
        shiny::req(plot_data())

        plot_data() %>%
          dplyr::filter(.data$feature == input$feature_choice) %>%
          dplyr::select("sample", "x", "y")
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
