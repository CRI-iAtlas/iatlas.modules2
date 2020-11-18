ui <- function() {
  shiny::fluidPage(

    shiny::titlePanel("Availible Modules"),
    shiny::mainPanel(
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Barplots",
          barplot_ui("barplot1", title = "Example 1"),
          barplot_ui("barplot2", title = "Example 2"),
          barplot_ui("barplot3", title = "Example 3"),
          barplot_ui("barplot4", title = "Example 4")
        )
      )
    )
  )
}
