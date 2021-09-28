
#
# ### cohort as input -----------------------------------------------------------
# ## extract from cohort --------------------------------------------------------
#
# test_that("get_cohort_feature_class_list", {
#   res1 <- get_cohort_feature_class_list(get_pcawg_immune_subtype_cohort_obj())
#   expect_vector(res1)
# })
#
# ## check cohort for module display --------------------------------------------
#
# test_that("cohort_has_features", {
#   expect_true(cohort_has_features(
#     get_tcga_immune_subtype_cohort_obj_50(),
#     c("leukocyte_fraction", "Stromal_Fraction", "Tumor_fraction")
#   ))
#   expect_false(cohort_has_features(
#     get_pcawg_immune_subtype_cohort_obj(),
#     c("leukocyte_fraction", "Stromal_Fraction", "Tumor_fraction")
#   ))
#   expect_true(cohort_has_features(
#     get_tcga_immune_subtype_cohort_obj_50(),
#     c("leukocyte_fraction", "Stromal_Fraction", "not_a_feature"),
#     all_features = F
#   ))
#   expect_false(cohort_has_features(
#     get_tcga_immune_subtype_cohort_obj(),
#     c("not_a_feature"),
#     all_features = F
#   ))
# })
#
# test_that("cohort_has_classes", {
#   expect_true(cohort_has_classes(
#     get_tcga_immune_subtype_cohort_obj_50(),
#     c("DNA Alteration", "TIL Map Characteristic")
#   ))
#   expect_false(cohort_has_classes(
#     get_pcawg_immune_subtype_cohort_obj(),
#     c("DNA Alteration", "TIL Map Characteristic")
#   ))
#   expect_true(cohort_has_classes(
#     get_tcga_immune_subtype_cohort_obj_50(),
#     c("DNA Alteration", "not_a_class"),
#     all_classes = F
#   ))
#   expect_false(cohort_has_classes(
#     get_tcga_immune_subtype_cohort_obj(),
#     c("not_a_class"),
#     all_classes = F
#   ))
# })
#
# ## API queries --------------------------------------------------------------
# # features ------------------------------------------------------------------
#
# test_that("query_feature_values_with_cohort_object", {
#
#   expected_columns <- c(
#     "sample",
#     "feature_name",
#     "feature_display",
#     "feature_value",
#     "feature_order",
#     "feature_class"
#   )
#
#   result1 <- query_feature_values_with_cohort_object(
#     get_pcawg_immune_subtype_cohort_obj(),
#     features = "Lymphocytes_Aggregate1"
#   )
#   expect_named(result1, expected_columns)
#   expect_length(result1$feature_value, 455L)
#
#   result2 <- query_feature_values_with_cohort_object(
#     get_pcawg_immune_subtype_cohort_obj(),
#     feature_classes  = "Overall Proportion"
#   )
#   expect_named(result2, expected_columns)
#   expect_length(result2$feature_value, 0)
#
#   result3 <- query_feature_values_with_cohort_object(
#     get_pcawg_immune_subtype_cohort_obj(),
#     feature_classes = "EPIC"
#   )
#   expect_named(result3, expected_columns)
#   expect_length(result3$feature_value, 3640L)
#
#   result4 <- query_feature_values_with_cohort_object(
#     get_pcawg_feature_bin_cohort_obj(),
#     feature_classes = "EPIC"
#   )
#
#   result5 <- query_feature_values_with_cohort_object(
#     get_tcga_immune_subtype_cohort_obj_50(),
#     features = "OS_time"
#   )
#
#   result6 <- query_feature_values_with_cohort_object(
#     list(
#       "dataset" = "PCAWG",
#       "group_name" = "COAD",
#       "group_type" = "User Defined Group",
#       "sample_tbl" = iatlas.api.client::query_dataset_samples(datasets = "PCAWG") %>%
#         dplyr::rename("sample" = "sample_name")
#     ),
#     features = "Lymphocytes_Aggregate1"
#   )
#
#   result7 <- query_feature_values_with_cohort_object(
#     get_pcawg_immune_subtype_cohort_obj(),
#     features = "Lymphocytes_Aggregate1",
#     groups = "C1"
#   )
#   expect_named(result7, expected_columns)
#   expect_true(length(result7$feature_value) < 455L)
#   expect_true(length(result7$feature_value) > 0L)
# })
#
# # genes ---------------------------------------------------------------------
#
# test_that("query_gene_expression_with_cohort_object", {
#
#   expected_columns <- c(
#     "sample",
#     "entrez",
#     "hgnc",
#     "rna_seq_expr"
#   )
#   result1 <- query_gene_expression_with_cohort_object(
#     get_pcawg_immune_subtype_cohort_obj(),
#     entrez = 135L
#   )
#
#   expect_named(result1, expected_columns)
#   expect_equal(nrow(result1), 455L)
# })
