read_rds_file <- function(file){
  test_file_dir  <- system.file("test_rds_files", package = "iatlas.modules2")
  readRDS(file.path(test_file_dir, file))
}

#TODO: fix using datasets
tcga_samples_tbl <- iatlas.api.client::query_cohort_samples(cohorts = "TCGA_TCGA_Study")
tcga_samples <- tcga_samples_tbl$sample_name
tcga_features_tbl <- iatlas.api.client::query_features(cohorts = "TCGA")

#TODO: fix using datasets
pcawg_samples_tbl <- iatlas.api.client::query_cohort_samples(cohorts = "PCAWG_PCAWG_Study")
pcawg_samples <- pcawg_samples_tbl$sample_name
pcawg_features_tbl <- iatlas.api.client::query_features(cohorts = "PCAWG")


