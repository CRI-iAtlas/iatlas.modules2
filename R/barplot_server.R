
#' Barplot Server
#'
#' @param id Module ID
#' @param plot_data A shiny::reactive that returns a dataframe with columns
#' "sample", "group", "feature", "feature_value", and optional columns,
#' "group_description", and "feature_class"
#' @param barplot_xlab A shiny::reactive that returns a string
#' @param barplot_ylab A shiny::reactive that returns a string
#' @param barplot_title A shiny::reactive that returns a string
#' @param barplot_label A shiny::reactive that returns a string
#' @param drilldown A shiny::reactive that returns True or False
#' @param ... shiny:reactives passed to drilldown_scatterplot_server
#'
#' @export
#' @importFrom magrittr %>%
barplot_server <- function(
  id,
  plot_data,
  barplot_xlab  = shiny::reactive(""),
  barplot_ylab  = shiny::reactive(""),
  barplot_title = shiny::reactive(""),
  barplot_label = shiny::reactive("Feature"),
  drilldown     = shiny::reactive(F),
  ...
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      display_feature_class_selection_ui <- shiny::reactive({
        shiny::req(plot_data())
        display_barplot_feature_class_selection_ui(plot_data())
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

        shiny::req(display_feature_class_selection_ui())
        choices <- unique(plot_data()$feature_class)

        shiny::selectInput(
          inputId  = ns("feature_class_choice"),
          label    = "Select Feature Class",
          choices  = choices
        )
      })

      barplot_data <- shiny::reactive({
        shiny::req(plot_data())
        build_barplot_data(plot_data(), input$feature_class_choice)
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
          x_col = "group",
          y_col = "MEAN",
          color_col = "feature",
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

      group_data <- shiny::reactive({
        shiny::req("group_description" %in% colnames(plot_data()))
        plot_data() %>%
          dplyr::select("group", "description" = "group_description") %>%
          dplyr::distinct()
      })

      plotly_server(
        "barplot",
        plot_data = summarized_barplot_data,
        group_data = group_data,
        eventdata = barplot_eventdata
      )

      drilldown_scatterplot_server(
        "scatterplot",
        plot_data = barplot_data,
        eventdata = barplot_eventdata,
        ...
      )

      output$drilldown_ui <- shiny::renderUI({
        if(drilldown()) drilldown_scatterplot_ui(ns("scatterplot"))
      })
    }
  )
}
