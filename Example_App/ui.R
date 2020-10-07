ui <- fluidPage(

  titlePanel("Availible Modules"),
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Barplots",
        barplot_ui("barplot1", title = "Example 1"),
        barplot_ui("barplot2", title = "Example 2")
      ),
      tabPanel(
        "Distribution plots",
        distributions_plot_ui("distplot1", title = "Example1")
      )
    )
  )
)
