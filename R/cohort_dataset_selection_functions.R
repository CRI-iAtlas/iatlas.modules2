#' Create Cohort Module String
#'
#' @param .datasets a string(s) in the dataset column of the tibble
#' @param tbl A tibble with columns dataset, and module
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter pull
create_cohort_module_string <- function(.datasets, tbl = NULL){
    dataset_to_module_tbl <- dplyr::tribble(
        ~module,                  ~dataset,
        "Sample Group Overview",  "TCGA",
        "Tumor Microenvironment", "TCGA",
        "Immune Feature Trends",  "TCGA",
        "Clinical Outcomes",      "TCGA",
        "IO Targets",             "TCGA",
        "TIL Maps",               "TCGA",
        "Driver Associations",    "TCGA",
        "Sample Group Overview",  "PCAWG",
        "Tumor Microenvironment", "PCAWG",
        "Immune Feature Trends",  "PCAWG",
        "IO Targets",             "PCAWG"
    )

    if (is.null(tbl)) tbl <- dataset_to_module_tbl

    modules <- tbl %>%
        dplyr::filter(.data$dataset %in% .datasets) %>%
        dplyr::pull(.data$module)
    if (length(modules) == 0){
        return("No modules currently available for selected dataset")
    } else {
        msg <- modules %>%
            stringr::str_c(collapse = ", ") %>%
            stringr::str_c(
                "Specific analysis modules (displayed after you make your selection) are available for each dataset. ",
                "Modules available for dataset ",
                .datasets,
                ": ",
                .
            )
        return(msg)
    }

}

#Create a list of ICI datasets based on whether the data is RNA-Seq or Nanostring. Needs to be updated if more Nanostring data is added
create_ici_options <- function(.datasets){
    nanostring_ds <- c("Chen_CanDisc_2016", "Melero_GBM_2019", "Prat_CanRes_2017")
    rnaseq_nano_ds <- "Prins_GBM_2019" #this dataset has both RNA-Seq and Nanostring data available
    
    return(
        list(
            'RNA-Seq' = .datasets[!(.datasets %in% nanostring_ds)],
            'Nanostring' = .datasets[.datasets %in% c(nanostring_ds, rnaseq_nano_ds)]
        )
    )
}
