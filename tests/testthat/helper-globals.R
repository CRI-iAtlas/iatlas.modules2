get_tcga_samples_tbl <- memoise::memoise(
  function(){
    iatlasGraphQLClient::query_cohort_samples(cohorts = "TCGA")
  }
)

get_tcga_study_samples_tbl <- memoise::memoise(
  function(){
    iatlasGraphQLClient::query_cohort_samples(cohorts = "TCGA_TCGA_Study")
  }
)

get_tcga_immune_subtypes_samples_tbl <- memoise::memoise(
  function(){
    iatlasGraphQLClient::query_cohort_samples(cohorts = "TCGA_Immune_Subtype")
  }
)

get_pcawg_gender_samples_tbl <- memoise::memoise(
  function(){
    iatlasGraphQLClient::query_cohort_samples(cohorts = "PCAWG_Gender")
  }
)

get_ici_samples_tbl <- memoise::memoise(
  function(){
    cohorts <-  c("Gide_Cell_2019_Responder", "HugoLo_IPRES_2016_Responder")
    iatlasGraphQLClient::query_cohort_samples(cohorts = cohorts)
  }
)

get_tcga_samples <- memoise::memoise(
  function(){
    get_tcga_samples_tbl()$sample_name
  }
)

get_tcga_samples_50 <- memoise::memoise(
  function(){
    get_tcga_samples()[1:50]
  }
)

get_tcga_features_tbl <- memoise::memoise(
  function(){
    iatlasGraphQLClient::query_features(cohorts = "TCGA")
  }
)

get_ici_features_tbl <- memoise::memoise(
  function(){
    iatlasGraphQLClient::query_features(cohorts = c("Gide_Cell_2019_Responder", "HugoLo_IPRES_2016_Responder"))
  }
)

get_pcawg_samples_tbl <- memoise::memoise(
  function(){
    iatlasGraphQLClient::query_cohort_samples(cohorts = "PCAWG")
  }
)

get_pcawg_study_samples_tbl <- memoise::memoise(
  function(){
    iatlasGraphQLClient::query_cohort_samples(cohorts = "PCAWG_PCAWG_Study")
  }
)

get_pcawg_samples <- memoise::memoise(
  function(){
    get_pcawg_samples_tbl()$sample_name
  }
)

get_pcawg_samples_50 <- memoise::memoise(
  function(){
    get_pcawg_samples()[1:50]
  }
)

get_pcawg_features_tbl <- memoise::memoise(
  function(){
    iatlasGraphQLClient::query_features(cohorts = "PCAWG")
  }
)

# ----

get_user_group_tbl <- memoise::memoise(
  function(){
    dir  <- system.file("csv", package = "iatlas.modules2")
    readr::read_csv(file.path(dir, "test_user_group.csv"))
  }
)


read_rds_file <- function(file){
  test_file_dir  <- system.file("test_rds_files", package = "iatlas.modules2")
  readRDS(file.path(test_file_dir, file))
}

get_pcawg_immune_subtype_cohort_obj <- memoise::memoise(
  function(){
    read_rds_file("pcawg_immune_subtype_cohort_obj.rds")
  }
)

get_tcga_immune_subtype_cohort_obj <- memoise::memoise(
  function(){
    read_rds_file("tcga_immune_subtype_cohort_obj.rds")
  }
)

get_tcga_immune_subtype_cohort_obj_small <- memoise::memoise(
  function(){
    read_rds_file("tcga_immune_subtype_cohort_obj_small.rds")
  }
)

get_tcga_tcga_study_cohort_obj <- memoise::memoise(
  function(){
    read_rds_file("tcga_tcga_study_cohort_obj.rds")
  }
)

get_ici_responder_cohort_obj <- memoise::memoise(
  function(){
    read_rds_file("ici_responder_cohort_obj.rds")
  }
)
get_tcga_mutation_cohort_obj_small <- memoise::memoise(
  function(){
    read_rds_file("tcga_mutation_cohort_obj_small.rds")
  }
)
get_tcga_bin_cohort_obj_small <- memoise::memoise(
  function(){
    read_rds_file("tcga_bin_cohort_obj_small.rds")
  }
)




