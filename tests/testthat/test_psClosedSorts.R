context("psClosedSorts class")

# construction ====
test_that(desc = "construction produces", code = {
  expect_s3_class(object = csorts, class = c("psClosedSorts", "matrix"))
})

# validation ====
test_that(desc = "validation against 'items' works", code = {
  expect_error(x = assert_S3(x = csorts, items = c(foo = "foo")))
})

test_that(desc = "validation against 'grid' works", code = {
  # this is reusing the tests from other functions, so no need to run a lot of tests here
  expect_error(
    object = assert_S3(x = csorts, grid = grid_byhand[1, ])
  )
})

# coercion ====
#TODO below should be in helper, but won't work there because of loading order or something
csorts_from_one_sort <- as_psClosedSorts(obj = one_sort)
# this also works for psSort with offsets
csorts_from_one_sort_hex <- as_psClosedSorts(obj = one_sort_from_vec_hex)

test_that(desc = "coercion produces valid 'psClosedSorts'", code = {
  # you can coerce a single "row" of psClosedSorts from psSort
  x <- list(
    csorts_from_one_sort = csorts_from_one_sort,
    csorts_from_one_sort_hex = csorts_from_one_sort_hex
  )
  iwalk(.x = x, .f = function(x, y) {
    expect_s3_class(object = x, class = c("psClosedSorts"))
    expect_S3(x = x, label = y)
  })
})

test_that(desc = "coercion from 'psSort' works", code = {
  expect_equivalent(
    object = csorts_from_one_sort,
    expected = psClosedSorts(
      csorts = matrix(
        data = c(1, 2),
        nrow = 1
      )
    )
  )
  expect_equivalent(
    object = colnames(csorts_from_one_sort),
    expected = c("live_2_work", "work_2_live")
  )
  expect_equivalent(
    object = csorts_from_one_sort_hex,
    expected = psClosedSorts(
      csorts = matrix(
        data = c(-2, 0, 3, 2),
        nrow = 1
      )
    )
  )
  expect_equivalent(
    object = colnames(csorts_from_one_sort_hex),
    expected = c("foo", "bar", "zong", "zap")
  )
})



