test_that("assert_correct_synoptics validates input correctly", {
  # Test default case
  expect_no_error(assert_correct_synoptics("default"))

  # Test valid custom synoptics
  valid_synoptics <- reference_synoptics
  expect_no_error(assert_correct_synoptics(valid_synoptics))

  # Test missing columns
  invalid_synoptics <- valid_synoptics[, -1]
  expect_error(
    assert_correct_synoptics(invalid_synoptics),
    "missing required columns"
  )

  # Test invalid data types
  invalid_types <- valid_synoptics
  invalid_types$fraction <- as.character(invalid_types$fraction)
  expect_error(
    assert_correct_synoptics(invalid_types),
    "incorrect data type"
  )
})
