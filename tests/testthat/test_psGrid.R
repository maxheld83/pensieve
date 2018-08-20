context("psGrid class")

test_that(desc = "coercion from integer vector works", code = {
  expect_s3_class(object = grid_bycoercion, class = c("psGrid", "matrix"))
  expect_equivalent(object = grid_bycoercion, expected = grid_byhand)
})
test_that(desc = "coercion from logical vector works", code = {
  # very unlikely case, can happen accidentally when subsetting some grid matrix
  from_logical_vector <- as_psGrid(obj = c(TRUE, FALSE, FALSE))
  expect_s3_class(object = from_logical_vector, class = c("psGrid", "matrix"))
  expect_S3(x = from_logical_vector)
})
