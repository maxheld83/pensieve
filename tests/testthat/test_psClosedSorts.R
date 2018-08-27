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
test_that(desc = "coercion produces valid 'psClosedSorts'", code = {
  # you can coerce a single "row" of psClosedSorts from psSort
  x <- list(
    csorts_from_one_sort = csorts_from_one_sort,
    csorts_from_one_sort_hex = csorts_from_one_sort_hex,
    csorts_w_items = csorts_w_items
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
        data = c(2, 4, 7, 6),
        nrow = 1
      )
    )
  )
  expect_equivalent(
    object = colnames(csorts_from_one_sort_hex),
    expected = c("foo", "bar", "zong", "zap")
  )
  expect_equal(
    object = colnames(csorts_w_items),
    expected = c("work_2_live", "live_2_work", "enjoy_work"),
    info = "Expect correct item order."
  )
  expect_equivalent(
    object = csorts_w_items,
    expected = psClosedSorts(
      csorts = matrix(
        data = c(2, 1, NA),
        nrow = 1
      )
    )
  )
  expect_equivalent(
    object = {
      sort_with_cosmetic_colnames <- psSort(sort = matrix(data = c("foo", "bar"), ncol = 2, dimnames = list(NULL, cols = c("col1", "col2"))))
      as_psClosedSorts(obj = sort_with_cosmetic_colnames)
    },
    expected = psClosedSorts(csorts = matrix(data = c(1,2), ncol = 2, dimnames = list(NULL, c("foo", "bar"))))
  )
})



