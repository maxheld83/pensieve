context("psClosedSorts class")

test_that(desc = "construction works", code = {
  expect_s3_class(object = csorts, class = c("psClosedSorts", "matrix"))
  expect_s3_class(object = csorts_multiple_conds, class = c("psClosedSorts", "array"))
})

test_that(desc = "validation against 'items' works", code = {
  expect_error(x = assert_S3(x = csorts, items = c(foo = "foo")))
})

test_that(desc = "validation against 'grid' works", code = {
  # this is reusing the tests from other functions, so no need to run a lot of tests here
  expect_error(
    object = assert_S3(x = csorts_multiple_conds, grid = grid_byhand[1, ])
  )
})

