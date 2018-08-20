context(desc = "psSort class")

# construction ====
test_that(desc = "construction and coercion of sorts yields proper psSort", code = {
  x <- list(
    one_sort = one_sort,
    one_sort_from_grid = one_sort_from_grid,
    one_sort_from_vec = one_sort_from_vec,
    one_sort_from_vec_hex = one_sort_from_vec_hex,
    one_sort_from_df = one_sort_from_df,
    one_sort_from_narrow_m1 = one_sort_from_narrow_m1,
    one_sort_from_narrow_m2 = one_sort_from_narrow_m2,
    one_sort_from_m3 = one_sort_from_m3
  )
  iwalk(.x = x, .f = function(x, y) {
    expect_s3_class(object = x, class = c("psSort","matrix"))
    expect_S3(x = x, label = y)
  })
})

# validation ====
test_that(desc = "validation errors out when 'grid' is not of same rank", code = {
  x <- psSort(matrix(data = c("foo", "bar", NA, NA), nrow = 2))
  expect_error(object = assert_S3(x, grid = grid_byhand))
})

test_that(desc = "validation errors out when there are too few cells for 'items'", code = {
  x <- psSort(matrix(data = "foo", nrow = 1))
  expect_error(object = assert_S3(x = x, items = items_text_en))
})

test_that(desc = "sorts with duplicate items error out", code = {
  x <- matrix(data = c("foo", "foo", NA, NA), nrow = 2)
  expect_error(object = psSort(x))
})

test_that(desc = "sorts with items in disallowed cells error out", code = {
  x <- psSort(matrix(data = c("foo", "bar", NA, NA, NA, NA), nrow = 2))
  expect_error(object = assert_S3(x = x, grid = grid_byhand))
})

test_that(desc = "sorts with item not in items error out", code = {
  x <- psSort(matrix(data = c("zap")))
  expect_error(object = assert_S3(x = x, items = as_psItemContent.character(obj = c(foo = "foo"))))
})

# coercion ====
test_that(desc = "coercion from grid works", code = {
  expect_matrix(
    x = one_sort_from_grid,
    nrows = nrow(grid_byhand),
    ncols = ncol(grid_byhand)
  )
})

test_that(desc = "coercion from integer(ish) vector works", code = {
  expect_equivalent(
    object = one_sort_from_vec,
    expected = psSort(
      matrix(
        data = c(NA, NA, "zong", "foo", "bar", "zap"),
        nrow = 2,
        byrow = TRUE
      )
    )
  )
  # should warn on unnamed integers
  expect_warning(object = as_psSort(obj = c(1,1,2,0)))
  # should message on empty columns
  expect_message(object = as_psSort(obj = c(foo = 1, zap = 3)))
  # should pass on non-default attributes
  expect_identical(object = one_sort_from_vec_hex %@% "polygon", expected = "hexagon")
  expect_identical(object = one_sort_from_vec_hex %@% "offset", expected = "odd")
})

test_that(desc = "coercion from long data.frame works", code = {
  expect_equivalent(
    object = one_sort_from_df,
    expected = psSort(
      matrix(
        data = c("foo", NA, "bar"),
        nrow = 1
      )
    )
  )
})

test_that(desc = "coercion from matrix works", code = {
  expect_equivalent(
    object = one_sort_from_narrow_m1,
    expected = psSort(
      matrix(
        data = c(NA, "bar", NA, NA, "foo", NA),
        nrow = 2,
        byrow = TRUE
      )
    )
  )
  expect_equivalent(
    object = one_sort_from_narrow_m2,
    expected = psSort(
      matrix(
        data = c(NA, "bar", NA, NA, "foo", "zap"),
        nrow = 2,
        byrow = TRUE
      )
    )
  )

  # place narrower grid in wider grid
  # wider m can't go in narrower grid
  expect_error(
    object = as_psSort(
      obj = matrix(
        data = LETTERS[1:8],
        ncol = 4
      ),
      grid = grid_byhand
    )
  )
  # must have index arg
  expect_error(
    object = as_psSort(
      obj = m2,
      grid = grid_byhand
      # no index arg should error out
    )
  )
  # index arg must not be too high
  expect_error(
    object = as_psSort(
      obj = m2,
      grid = grid_byhand,
      insert_at_grid_col = 3
    )
  )

  # intersperse matrix correctly given some grid
  expect_equivalent(
    object = one_sort_from_m3,
    expected = psSort(
      matrix(
        data = c("foo", NA, "bar", NA, NA, "zap", NA, "zong", NA),
        nrow = 3
      )
    )
  )
  expect_error(
    object = {
      m3[1, 1] <- "wop"  # this is too many items for allowed cells
      as_psSort(
        obj = m3,
        grid = grid2
      )
    }
  )
})
