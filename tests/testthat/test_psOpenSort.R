context("psOpenSort")

test_that(desc = "construction of single logical open sort works", code = {
  class <- c("psLogicalOpenSort", "psOpenSort", "matrix")
  expect_s3_class(object = lisa, class = class)
  expect_s3_class(object = peter, class = class)
  expect_s3_class(object = rebecca, class = class)
  expect_s3_class(object = ira, class = class)
})

test_that(desc = "construction of single ordinal open sort works", code = {
  expect_s3_class(object = tyler, class = c("psOrdinalOpenSort", "psOpenSort", "matrix"))
  expect_integer(x = tyler)
})

test_that(desc = "construction of single interval open sort works", code = {
  expect_s3_class(object = roberta, class = c("psIntervalOpenSort", "psOpenSort", "matrix"))
  expect_numeric(x = roberta)
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
  expect_warning(object = as_psOpenSort(osort = losort, descriptions = descriptions))
  expect_var(x = ira)
})
