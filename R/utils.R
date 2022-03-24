utils::globalVariables(c("val", "<<-", "label"))

# system files ----------------------------------------------------------------

#' Get System Path File
#'
#' @param prefix A string, the file prefix
#' @param extension A the string, the file extension
#' @param folder A string, the file's folder
#' @param package A string, the package the file is in
get_system_path_file <- function(
  prefix, extension, folder, package = "iatlasModules2"
){
  file_name <- stringr::str_c(prefix, extension)
  file.path(system.file(folder, package = package), file_name)
}

get_markdown_path <- function(name, extension = ".markdown"){
  get_system_path_file(name, extension, "markdown")
}

# ----

dedupe <- function(r) {
  shiny::makeReactiveBinding("val")
  shiny::observe(val <<- r(), priority = 10)
  shiny::reactive(val)
}


safe_is_na <- function(item){
  !is.list(item) && is.na(item)
}
