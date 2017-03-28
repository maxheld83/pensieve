context("Parallel analysis")

test_that(desc = "works with some free distribution data",
          code = {
  values <- c(-2:2)
  grid <- rep(x = length(values), times = 5)
  names(grid) <- as.character(values)
  n <- 20
  p <- 10
  res <- draw_rand_sort(grid = grid, n = n, p = p)
  expect_matrix(x = res,
                mode = "integer",
                any.missing = FALSE,
                all.missing = FALSE,
                nrows = p,
                ncols = n,
                null.ok = FALSE)
  expect_true(object = diag_inside_grid(sorts = res, grid = grid),
              info = "Random data inside grid")
})

test_that(desc = "works with some forced distribution data",
          code = {
  values <- c(-2:2)
  grid <- as.integer(c(2,4,6,4,2))
  names(grid) <- as.character(values)
  n <- 20
  p <- 18 # == sum(grid)
  res <- draw_rand_sort(grid = grid, n = n, p = p)
  expect_matrix(x = res,
                mode = "integer",
                any.missing = FALSE,
                all.missing = FALSE,
                nrows = p,
                ncols = n,
                null.ok = FALSE)
  expect_true(object = diag_inside_grid(sorts = res, grid = grid),
              info = "Random data inside grid"
  )
})

test_that(desc = "works with civicon_2014",
          code = {
  data("civicon_2014")
  dataset <- civicon_2014$qData$sorts[,,"before"]
  res <- run_parallel(dataset = dataset,
                      centile = .95,
                      runs = 10,
                      grid = diag_distro_max(sorts = dataset))
  expect_vector(x = res,
                strict = TRUE,
                any.missing = FALSE,
                all.missing = FALSE,
                len = 18,
                null.ok = FALSE)
})
