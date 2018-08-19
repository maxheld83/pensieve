context("psClosedSorts class")

test_that(desc = "construction works", code = {
  expect_s3_class(object = csorts, class = c("psClosedSorts", "matrix"))
  expect_s3_class(object = csorts_multiple_conds, class = c("psClosedSorts", "array"))
})

test_that(desc = "validation against 'grid' works", code = {
  skip("in dev")
  bad_sorts <- matrix(
    data = c(
      -1, 0, 1, # correct
      -1, -1, 0,  # incorrect, FALSE cell occupied
      -1, -1, -1  # out of range of grid
    ),
    nrow = 3,
    byrow = TRUE
  )
  bad_sorts <- psClosedSorts(csorts = bad_sorts)
  validate_S3(x = bad_sorts, grid = grid_bycoercion)
})
