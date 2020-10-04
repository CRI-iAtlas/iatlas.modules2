
#' Barplot Server
#'
#' @param id Module ID
#' @param plot_data A shiny::reactive that returns a dataframe with columns
#' "sample", "x", "y", "color"
#' @param group_data A shiny::reactive that returns NULL or a dataframe with
#' columns "feature", "description"
#' @param feature_data A shiny::reactive that returns NULL or a dataframe with
#' columns "feature", "class"
#' @param barplot_xlab A shiny::reactive that returns a string
#' @param barplot_ylab A shiny::reactive that returns a string
#' @param barplot_title A shiny::reactive that returns a string
#' @param barplot_label A shiny::reactive that returns a string
#' @param drilldown A shiny::reactive that returns True or False
#'
#' @export
#' @importFrom magrittr %>%
barplot_server <- function(
  id,
  plot_data,
  group_data    = shiny::reactive(NULL),
  feature_data  = shiny::reactive(NULL),
  barplot_xlab  = shiny::reactive(""),
  barplot_ylab  = shiny::reactive(""),
  barplot_title = shiny::reactive(""),
  barplot_label = shiny::reactive("Feature"),
  drilldown     = shiny::reactive(F)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$feature_class_selection_ui <- shiny::renderUI({
        shiny::req(feature_data())
        choices <- unique(feature_data()$class)
        shiny::req(length(choices) > 1)
        shiny::selectInput(
          inputId  = ns("feature_class_choice"),
          label    = "Select Feature Class",
          choices  = choices
        )
      })

      barplot_features <- shiny::reactive({
        shiny::req(input$feature_class_choice)
        feature_data() %>%
          dplyr::filter(.data$class == input$feature_class_choice) %>%
          dplyr::pull("feature")
      })

      barplot_data <- shiny::reactive({
        shiny::req(plot_data())
        plot_data <- dplyr::select(plot_data(), "sample", "x", "y", "color")

        if(!is.null(input$feature_class_choice)){
          shiny::req(barplot_features())
          plot_data <- plot_data %>%
            dplyr::filter(.data$color %in% barplot_features())
        }
        return(plot_data)
      })

      summarized_barplot_data <- shiny::reactive({
        shiny::req(barplot_data(), barplot_label())
        summarise_barplot_se(barplot_data(), barplot_label())
      })

      barplot_source_name <- shiny::reactive(ns("barplot"))

      output$barplot <- plotly::renderPlotly({
        shiny::req(summarized_barplot_data(), barplot_source_name())
        plotly_bar(
          summarized_barplot_data(),
          source_name = barplot_source_name(),
          y_col = "MEAN",
          color_col = "color",
          error_col = "SE",
          text_col = "text",
          xlab = barplot_xlab(),
          ylab = barplot_ylab(),
          title = barplot_title(),
        )
      })

      barplot_eventdata <- shiny::reactive({
        eventdata <- plotly::event_data("plotly_click", barplot_source_name())
        shiny::validate(shiny::need(eventdata, "Click on above barplot."))
        return(eventdata)
      })

      plotly_server(
        "barplot",
        plot_data = summarized_barplot_data,
        group_data = group_data,
        eventdata = barplot_eventdata
      )

      drilldown_scatterplot_server(
        "scatterplot",
        data = barplot_data,
        eventdata = barplot_eventdata
      )

      output$drilldown_ui <- shiny::renderUI({
        if(drilldown()) drilldown_scatterplot_ui(ns("scatterplot"))
      })
    }
  )
}
