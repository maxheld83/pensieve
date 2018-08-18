context(desc = "psSort class")

test_that(desc = "construction of sorts works", code = {
  expect_s3_class(object = one_sort, class = c(
    "psSort",
    "matrix"
  ))
})

test_that(desc = "validation errors out when 'grid' is not of same rank", code = {
  x <- psSort(matrix(data = c("foo", "bar", NA, NA), nrow = 2))
  expect_error(object = validate_S3(x, grid = grid_byhand))
})

test_that(desc = "validation errors out when there are too few cells for 'items'", code = {
  x <- psSort(matrix(data = "foo", nrow = 1))
  expect_error(object = validate_S3(x = x, items = items_text_en))
})

test_that(desc = "sorts with duplicate items error out", code = {
  x <- matrix(data = c("foo", "foo", NA, NA), nrow = 2)
  expect_error(object = psSort(x))
})

test_that(desc = "sorts with items in disallowed cells error out", code = {
  x <- psSort(matrix(data = c("foo", "bar", NA, NA, NA, NA), nrow = 2))
  expect_error(object = validate_S3(x = x, grid = grid_byhand))
})

test_that(desc = "sorts with item not in items error out", code = {
  x <- psSort(matrix(data = c("zap")))
  expect_error(object = validate_S3(x = x, items = as_psItemContent.character(obj = c(foo = "foo"))))
})

test_that(desc = "coercion from grid works", code = {
  x <- as_psSort(obj = grid_byhand)
  expect_s3_class(object = x, class = c("psSort", "matrix"))
  expect_S3(x = x)
  expect_matrix(
    x = x,
    nrows = nrow(grid_byhand),
    ncols = ncol(grid_byhand)
  )
})
