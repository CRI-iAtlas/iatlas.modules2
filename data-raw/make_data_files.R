pcawg_immune_subtype_cohort_obj <- readRDS("inst/test_rds_files/pcawg_immune_subtype_cohort_obj.rds")
tcga_immune_subtype_cohort_obj <- readRDS("inst/test_rds_files/tcga_immune_subtype_cohort_obj.rds")
tcga_immune_subtype_cohort_obj_small <- readRDS("inst/test_rds_files/tcga_immune_subtype_cohort_obj_small.rds")
tcga_tcga_study_cohort_obj <- readRDS("inst/test_rds_files/tcga_tcga_study_cohort_obj.rds")
ici_responder_cohort_obj <- readRDS("inst/test_rds_files/ici_responder_cohort_obj.rds")
tcga_mutation_cohort_obj_small <- readRDS("inst/test_rds_files/tcga_mutation_cohort_obj_small.rds")
tcga_bin_cohort_obj_small <- readRDS("inst/test_rds_files/tcga_bin_cohort_obj_small.rds")
upload_cohort_obj <- readRDS("inst/test_rds_files/upload_cohort_obj.rds")

usethis::use_data(pcawg_immune_subtype_cohort_obj, overwrite = TRUE)
usethis::use_data(tcga_immune_subtype_cohort_obj, overwrite = TRUE)
usethis::use_data(tcga_immune_subtype_cohort_obj_small, overwrite = TRUE)
usethis::use_data(tcga_tcga_study_cohort_obj, overwrite = TRUE)
usethis::use_data(ici_responder_cohort_obj, overwrite = TRUE)
usethis::use_data(tcga_mutation_cohort_obj_small, overwrite = TRUE)
usethis::use_data(tcga_bin_cohort_obj_small, overwrite = TRUE)
usethis::use_data(upload_cohort_obj, overwrite = TRUE)

