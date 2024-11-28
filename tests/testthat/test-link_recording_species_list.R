# tests/testthat/test-link_recording_species_list.R

test_that("invalid source parameter throws error", {
  test_data <- tibble(species = c("Quercus robur", "Fagus sylvatica"))
  expect_error(
    link_recording_species_list(test_data, "species", source = "invalid"),
    "invalid source parameter"
  )
})

test_that("non-existent column throws error", {
  test_data <- tibble(species = c("Quercus robur", "Fagus sylvatica"))
  expect_error(
    link_recording_species_list(test_data, "wrong_column", source = "gbif_backbone"),
    "col_name does not exist in the dataset"
  )
})

test_that("max_gbif limit works", {
  # Create test data with more species than max_gbif
  test_data <- tibble(species = paste0("Species_", 1:6000))
  expect_error(
    link_recording_species_list(test_data, "species", source = "gbif_backbone", max_gbif = 5000),
    "higer than allowed in max_gbif"
  )
})

test_that("data.frame source works correctly", {
  test_data <- tibble(species = c("Quercus robur", "Fagus sylvatica"))
  test_taxons <- tibble(
    name = c("Quercus robur", "Fagus sylvatica"),
    usageKey = c(1, 2),
    acceptedUsageKey = c(1, 2)
  )

  result <- link_recording_species_list(
    test_data,
    "species",
    source = "data.frame",
    taxons = test_taxons
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("usageKey", "acceptedUsageKey") %in% colnames(result)))
})

test_that("included source checks for required columns", {
  # Test missing columns
  test_data <- tibble(species = c("Quercus robur", "Fagus sylvatica"))
  expect_error(
    link_recording_species_list(test_data, "species", source = "included"),
    "usageKey and acceptedUsageKey must be present in data"
  )

  # Test with required columns
  test_data_complete <- tibble(
    species = c("Quercus robur", "Fagus sylvatica"),
    usageKey = c(1, 2),
    acceptedUsageKey = c(1, 2)
  )
  expect_no_error(
    link_recording_species_list(test_data_complete, "species", source = "included")
  )
})

# Mock test for taxon_db source (you'll need to mock the database connection)
test_that("taxon_db source works with mock connection", {
  skip("Database connection tests not implemented")
  # Implementation would depend on your database structure
})

# Mock test for gbif_backbone source (you might want to mock rgbif calls)
test_that("gbif_backbone source works with mock data", {
  skip("GBIF API tests not implemented")
  # Implementation would depend on how you want to mock rgbif calls
})
