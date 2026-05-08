
test_that("clean_text rejects non-string inputs", {
  expect_error(clean_text(543))
})
