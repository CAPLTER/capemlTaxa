testthat::test_that("write_taxa_map writes taxa_map.csv and returns correct output", {

  sample_taxa <- data.frame(
    scientific_name = c("Homo sapiens", "Panthera leo", NA, "carrot")
  )

  if (base::file.exists("taxa_map.csv")) {
    base::file.remove("taxa_map.csv")
  }

  result <- capemlTaxa::write_taxa_map(sample_taxa, scientific_name)

  testthat::expect_true(base::file.exists("taxa_map.csv"))
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(base::nrow(result), base::nrow(sample_taxa))

  base::file.remove("taxa_map.csv")
})
