context("psClosedSorts class")

test_that(desc = "construction works", code = {
  expect_s3_class(object = csorts, class = c("psClosedSorts", "matrix"))
  expect_s3_class(object = csorts_multiple_conds, class = c("psClosedSorts", "array"))
})
