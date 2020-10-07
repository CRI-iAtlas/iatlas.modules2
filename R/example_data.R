utils::globalVariables("iris")

#' Example Starwars Data
example_starwars_data <- function(){
  dplyr::select(
    dplyr::starwars,
    "sample" = "name",
    "x" = "species",
    "color" = "gender",
    "y" = "height"
  )
}

#' Example Starwars Group Data
#' @importFrom magrittr %>%
example_starwars_group_data <- function(){
  dplyr::starwars %>%
    dplyr::select("group" = "species") %>%
    dplyr::distinct() %>%
    dplyr::mutate("description" = stringr::str_c("Species: ", .data$group))
}

#' Example Iris Data
#' @importFrom magrittr %>%
#' @importFrom rlang .data
example_iris_data <- function(){
  iris %>%
    dplyr::as_tibble() %>%
    dplyr::mutate("sample" = as.character(1:dplyr::n())) %>%
    tidyr::pivot_longer(!c("Species", "sample"), names_to = "feature", values_to = "y") %>%
    dplyr::mutate("color" = .data$feature) %>%
    dplyr::rename("x" = "Species")
}


#' Example Iris Group Data
#' @importFrom magrittr %>%
example_iris_group_data <- function(){
  example_iris_data() %>%
    dplyr::select("group" = "x") %>%
    dplyr::distinct() %>%
    dplyr::mutate("description" = stringr::str_c("Species: ", .data$group))
}

#' Example Iris Feature Data
examplre_iris_feature_data <- function(){
  dplyr::tribble(
    ~class,   ~feature,        ~display,
    "Length", "Sepal.Length",  "Sepal Length",
    "Width",  "Sepal.Width",   "Sepal Width",
    "Length", "Petal.Length",  "Petal Length",
    "Width",  "Petal.Width",   "Petal Width"
  )
}
