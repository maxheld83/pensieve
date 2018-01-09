context("psOpenSort")

test_that(desc = "construction of single open sort works", code = {
  expect_s3_class(object = lisa, class = c("psOpenSort", "matrix"))
  expect_s3_class(object = peter, class = c("psOpenSort", "matrix"))
  expect_s3_class(object = rebecca, class = c("psOpenSort", "matrix"))
})

test_that(desc = "coercion works", code = {
  expect_equal(
    object = peter_m,
    expected = peter
  )
  expect_equal(
    object = peter_df,
    expected = peter
  )
})
