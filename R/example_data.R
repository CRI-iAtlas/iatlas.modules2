utils::globalVariables("iris")

#' Example Starwars Data
example_starwars_data <- function(){
  dplyr::starwars %>%
    dplyr::select(
      "sample" = "name",
      "group" = "species",
      "height",
      "mass"
    ) %>%
    tidyr::pivot_longer(
      -c("sample", "group"), names_to = "feature", values_to = "feature_value"
    )
}

example_starwars_data_func <- function(.feature_class){
  example_starwars_data()
}

#' Example Iris Data
#' @importFrom magrittr %>%
example_iris_data <- function(){
  iris %>%
    dplyr::as_tibble() %>%
    dplyr::mutate("sample" = as.character(1:dplyr::n())) %>%
    tidyr::pivot_longer(
      !c("Species", "sample"),
      names_to = "feature",
      values_to = "feature_value"
    ) %>%
    dplyr::rename("group" = "Species") %>%
    dplyr::mutate(
      "group_description" = stringr::str_c("Iris Species: ", .data$group),
    ) %>%
    dplyr::inner_join(
      dplyr::tribble(
        ~feature_class, ~feature,
        "Length",       "Sepal.Length",
        "Width",        "Sepal.Width",
        "Length",       "Petal.Length",
        "Width",        "Petal.Width"
      ),
      by = "feature"
    )
}

example_iris_data_func <- function(.feature_class = NULL){
  iris_data <- example_iris_data()
  if (!is.null(.feature_class)){
    iris_data <- dplyr::filter(iris_data, .data$feature_class == .feature_class)
  }
  dplyr::select(iris_data, -"feature_class")
}

