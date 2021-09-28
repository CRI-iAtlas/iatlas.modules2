cnf1 <- CohortNumericFilter$new(
  "name" = "B_cells_Aggregate2",
  "min" = 0,
  "max" = .01
)

cnf2 <- CohortNumericFilter$new(
  "name" = "Neutrophils_Aggregate2",
  "min" = 0,
  "max" = .1
)

cnfs1 <- CohortFilterList$new(list(), type = "numeric")
cnfs2 <- CohortFilterList$new(list(cnf1), type = "numeric")

cgf1 <- CohortGroupFilter$new(
  "name" = "PCAWG_Study",
  "values" = c("BLCA-US", "BRCA-US", "CLLE-ES")
)

cgf2 <- CohortGroupFilter$new(
  "name" = "Immune_Subtype",
  "values" = c("C4", "C5", "C6")
)

cgfs1 <- CohortFilterList$new(list(), type = "group")
cgfs2 <- CohortFilterList$new(list(cgf2), type = "group")


cf1 <- CohortFilters$new(
  numeric_filters = cnfs1,
  group_filters = cgfs1
)

cf2 <- CohortFilters$new(
  numeric_filters = cnfs2,
  group_filters = cgfs2
)

tag_group1 <- TagGroup$new(
  dataset_name = "PCAWG", group_name = "Immune_Subtype"
)

tag_group2 <- TagGroup$new(
  dataset_name = "TCGA", group_name = "Immune_Subtype"
)

mutation_group <- MutationStatusGroup$new(
  dataset_name = "TCGA", mutation_name = "ABL1:(NS)"
)
bin_group <- FeatureBinGroup$new(
  dataset_name = "TCGA", feature_name = "B_cells_Aggregate2", feature_bins = 2
)


pcawg_immune_subtype_cohort_obj <- Cohort$new(
  "feature_tbl" = iatlas.api.client::query_features(cohorts = "PCAWG_Immune_Subtype"),
  "filter_object" = cf1,
  "group_object" = tag_group1
)

saveRDS(
  pcawg_immune_subtype_cohort_obj,
  "inst/test_rds_files/pcawg_immune_subtype_cohort_obj.rds",
  version = 2
)

tcga_immune_subtype_cohort_obj_small <- Cohort$new(
  "feature_tbl" = iatlas.api.client::query_features(cohorts = "TCGA_Immune_Subtype"),
  "filter_object" = cf2,
  "group_object" = tag_group2
)

saveRDS(
  tcga_immune_subtype_cohort_obj_small,
  "inst/test_rds_files/tcga_immune_subtype_cohort_obj_small.rds",
  version = 2
)

#
# pcawg_feature_bin_cohort_obj <- build_cohort_object(
#   dataset = "PCAWG",
#   samples = iatlas.api.client::query_dataset_samples("PCAWG")$name,
#   group_name = "Immune Feature Bins",
#   group_type = "custom",
#   bin_immune_feature = "B_cells_Aggregate2",
#   bin_number = 2
# )
#
# saveRDS(
#   pcawg_feature_bin_cohort_obj,
#   "inst/test_rds_files/pcawg_feature_bin_cohort_obj.rds",
#   version = 2
# )
#
# tcga_immune_subtype_cohort_obj <- build_cohort_object(
#   dataset = "TCGA",
#   samples = iatlas.api.client::query_dataset_samples("TCGA")$name,
#   group_name = "Immune_Subtype",
#   group_type = "tag"
# )
#
# saveRDS(
#   tcga_immune_subtype_cohort_obj,
#   "inst/test_rds_files/tcga_immune_subtype_cohort_obj.rds",
#   version = 2
# )
#
# tcga_immune_subtype_cohort_obj_50 <- build_cohort_object(
#   dataset = "TCGA",
#   samples = iatlas.api.client::query_dataset_samples("TCGA")$name[1:50],
#   group_name = "Immune_Subtype",
#   group_type = "tag"
# )
#
# saveRDS(
#   tcga_immune_subtype_cohort_obj_50,
#   "inst/test_rds_files/tcga_immune_subtype_cohort_obj_50.rds",
#   version = 2
# )
#
# tcga_study_cohort_obj_50 <- build_cohort_object(
#   dataset = "TCGA",
#   samples = iatlas.api.client::query_dataset_samples("TCGA")$name[1:50],
#   group_name = "TCGA_Study",
#   group_type = "tag"
# )
#
# saveRDS(
#   tcga_study_cohort_obj_50,
#   "inst/test_rds_files/tcga_study_cohort_obj_50.rds",
#   version = 2
# )
#
# tcga_feature_bin_cohort_obj <- build_cohort_object(
#   dataset = "TCGA",
#   samples = iatlas.api.client::query_dataset_samples("TCGA")$name,
#   group_name = "Immune Feature Bins",
#   group_type = "custom",
#   bin_immune_feature = "leukocyte_fraction",
#   bin_number = 2
# )
#
# saveRDS(
#   tcga_feature_bin_cohort_obj,
#   "inst/test_rds_files/tcga_feature_bin_cohort_obj.rds",
#   version = 2
# )
#
#
# tcga_feature_bin_cohort_obj_50 <- build_cohort_object(
#   dataset = "TCGA",
#   "samples" = iatlas.api.client::query_dataset_samples("TCGA")$name[1:50],
#   group_name = "Immune Feature Bins",
#   group_type = "custom",
#   bin_immune_feature = "leukocyte_fraction",
#   bin_number = 2
# )
#
# saveRDS(
#   tcga_feature_bin_cohort_obj_50,
#   "inst/test_rds_files/tcga_feature_bin_cohort_obj_50.rds",
#   version = 2
# )
