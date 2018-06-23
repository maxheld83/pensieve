context("psGrid class")

test_that(desc = "coercion from vector works", code = {
  expect_s3_class(object = grid_bycoercion, class = c("psGrid", "matrix"))
  expect_equivalent(object = grid_bycoercion, expected = grid_byhand)
})

