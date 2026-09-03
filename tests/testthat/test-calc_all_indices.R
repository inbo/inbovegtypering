test_that("calc_all_indices keeps legacy normalized aliases for downstream callers", {
  test_data <- data.frame(
    presence = c(TRUE, FALSE),
    pct_presence = c(0.8, 0.3),
    pct_value = c(0.6, 0),
    cover_if_present = c(0.7, 0.4)
  )

  result <- calc_all_indices(test_data, r = 0.5, s = 0.5, t = 0.5)

  expect_true(all(c("wrd", "inc", "llk") %in% colnames(result)))
  expect_equal(result$wrd, result$nrm_wrd)
  expect_equal(result$inc, result$nrm_inc)
  expect_equal(result$llk, result$nrm_llk)

  classification <- list(
    recording_id = "recording-1",
    results = cbind(data.frame(syntaxon = "S1"), result)
  )
  class(classification) <- "inbovegclassification"

  expect_no_error(print(classification))
})
