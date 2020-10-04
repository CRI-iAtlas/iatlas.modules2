titleBox <- function(title) {
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      background = "yellow",
      shiny::span(shiny::strong(title), style = "font-size:24px")
    )
  )
}

subTitleBox <- function(title) {
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      shiny::span(shiny::strong(title),style = "font-size:16px")
    )
  )
}

sectionBox <- function(..., title) {
  shiny::fluidRow(
    shinydashboard::box(
      ...,
      width = 12,
      title = title,
      solidHeader = TRUE,
      status = "warning",
      collapsible = TRUE
    )
  )
}

optionsBox <- function(...) {
  shinydashboard::box(..., background = "navy")
}

plotBox <- function(...) {
  shinydashboard::box(..., status = "warning")
}

tableBox <- function(...) {
  shinydashboard::box(..., status = "warning")
}

textBox <- function(...) {
  shinydashboard::box(..., status = "success")
}

messageBox <- function(...) {
  shinydashboard::box(..., status = "danger", background = "green")
}

imgLinkBox <- function(..., linkId, title, imgSrc, boxText, linkText) {
  shinydashboard::box(
    ...,
    title = shiny::span(title, style = "font-size:15px"),
    solidHeader = TRUE, status = "primary",
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::img(src = imgSrc, width = "100%")
      ),
      shiny::column(
        width = 8,
        shiny::p(boxText),
        shiny::actionButton(inputId = linkId, label = linkText)
      )
    )
  )
}
