context("Distribution diagnostics")

test_that(desc = "work with civicon_2014",
          code = {
  data("civicon_2014")
  sorts <- civicon_2014$qData$sorts[,,"before"]
  res <- diag_distros(sorts = sorts)
  expect_matrix(x = res,
                mode = "integer",
                any.missing = FALSE,
                all.missing = FALSE,
                nrows = ncol(sorts),
                ncols = sum(abs(range(sorts))) + 1,
                row.names = "unique",  # this does NOT test the content for correctness, hence below
                col.names = "unique",
                null.ok = FALSE)
  expect_identical(object = rownames(res),
                   expected = colnames(sorts))
  expect_identical(object = colnames(res),
                   expected = as.character(
                     c(min(sorts):max(sorts))))
  expect_integer(x = res,
                 lower = 0,
                 any.missing = FALSE,
                 all.missing = FALSE,
                 null.ok = FALSE)  # this tests for counts
  expect_equivalent(object = res[1, ],
                    expected = diag_distro_max(sorts))
  expect_true(object = diag_same(sorts = sorts),
              info = "diag_same with civicon_2014")

  res <- diag_distro_max(sorts = sorts)
  expect_atomic_vector(x = res,
                any.missing = FALSE,
                all.missing = FALSE,
                names = "unique")
  expect_integer(x = res,
                 lower = 0,
                 any.missing = FALSE,
                 all.missing = FALSE,
                 names = "unique",
                 null.ok = FALSE)
  expect_identical(object = names(res),
                   expected = as.character(
                     c(min(sorts):max(sorts))))

  expect_true(object = diag_inside_grid(sorts = sorts,
                                        grid = diag_distro_max(sorts)),
              info = "diag_inside_grid with civicon_2014")

  expect_true(object = diag_forced(sorts = sorts,
                                   grid = diag_distro_max(sorts)),
              info = "diag_inside_grid with civicon_2014")
})
