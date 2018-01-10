context("psOpenSort")

test_that(desc = "construction of single open sort works", code = {
  class <- c("psOpenSort", "matrix")
  expect_s3_class(object = lisa, class = class)
  expect_s3_class(object = peter, class = class)
  expect_s3_class(object = rebecca, class = class)
  expect_s3_class(object = ira, class = class)
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

test_that(desc = "coercion drops zero-var", code = {
  expect_warning(object = as_psOpenSort(osort = osort, descriptions = descriptions))
  expect_var(x = ira)
})
