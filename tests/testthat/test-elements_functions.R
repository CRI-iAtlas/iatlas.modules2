
test_that("Build Tag Filter Named List", {
    result1 <- build_tag_filter_list("Immune_Subtype", "TCGA")
    expect_equal(names(result1), c("C1", "C2", "C3", "C4", "C5", "C6"))
    expect_equal(unname(result1), c("C1", "C2", "C3", "C4", "C5", "C6"))
})

