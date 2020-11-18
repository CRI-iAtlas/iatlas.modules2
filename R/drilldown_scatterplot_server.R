
#' Drilldown Scatterplot Server
#'
#' @param id Module ID
#' @param plot_data A shiny::reactive that returns a dataframe with columns
#' "sample", "group", "feature", "feature_value"
#' @param eventdata A shiny::reactive that returns a dataframe with column
#' "key"
#' @param x_feature_input A shiny::reactive that returns a string
#' @param y_feature_input A shiny::reactive that returns a string
#'
#' @export
drilldown_scatterplot_server <- function(
  id,
  plot_data,
  eventdata,
  x_feature_input = NULL,
  y_feature_input = NULL
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      selected_group <- shiny::reactive({
        shiny::req(eventdata())
        eventdata()$key[[1]]
      })

      scatterplot_data <- shiny::reactive({
        shiny::req(
          plot_data(),
          selected_group(),
          selected_group() %in% plot_data()$group
        )
        build_scatterplot_data(plot_data(), selected_group())
      })

      scatterplot_feature_columns <- shiny::reactive({
        scatterplot_data() %>%
          colnames() %>%
          setdiff("sample")
      })

      display_feature_selection_ui <- shiny::reactive({
        all(
          any(is.null(x_feature_input), is.null(y_feature_input)),
          length(scatterplot_feature_columns()) > 2
        )
      })

      output$display_feature_selection_ui <- shiny::reactive({
        display_feature_selection_ui()
      })

      shiny::outputOptions(
        output,
        "display_feature_selection_ui",
        suspendWhenHidden = FALSE
      )

      output$x_feature_selection_ui <- shiny::renderUI({
        shiny::req(display_feature_selection_ui())
        choices <- scatterplot_feature_columns()

        shiny::selectInput(
          inputId  = ns("x_feature_choice"),
          label    = "Select X Feature",
          choices  = choices
        )
      })

      output$y_feature_selection_ui <- shiny::renderUI({
        shiny::req(display_feature_selection_ui(), input$x_feature_choice)
        choices <- scatterplot_feature_columns() %>%
          setdiff(input$x_feature_choice)

        shiny::selectInput(
          inputId  = ns("y_feature_choice"),
          label    = "Select Y Feature",
          choices  = choices
        )
      })

      if(is.null(x_feature_input)){
        x_feature <-
          shiny::reactive(
            get_scatterplot_x_feature(
              input$x_feature_choice,
              scatterplot_feature_columns()
            )
          )
      } else {
        x_feature <- x_feature_input
      }

      if(is.null(y_feature_input)){
        y_feature <-
          shiny::reactive(
            get_scatterplot_y_feature(
              input$y_feature_choice,
              scatterplot_feature_columns()
            )
          )
      } else {
        y_feature <- y_feature_input
      }

      formatted_scatterplot_data <- shiny::reactive({
        shiny::req(
          scatterplot_data(),
          x_feature(),
          y_feature(),
          selected_group(),
          x_feature() %in% colnames(scatterplot_data()),
          y_feature() %in% colnames(scatterplot_data())
        )

        format_scatterplot_data(
          scatterplot_data(), x_feature(), y_feature(), selected_group()
        )
      })

      output$scatterplot <- plotly::renderPlotly({
        plotly_scatter(
          formatted_scatterplot_data(),
          text_col = "text",
          xlab = x_feature(),
          ylab = y_feature(),
          title = selected_group(),
          identity_line = TRUE
        )
      })

      plotly_server(
        "scatterplot",
        plot_data = formatted_scatterplot_data
      )

    }
  )
}
