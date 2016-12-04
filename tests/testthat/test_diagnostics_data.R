context("Distribution diagnostics")

test_that(desc = "work with civicon_2014",
          code = {
  data("civicon_2014")
  dataset <- civicon_2014$qData$sorts[,,"before"]
  res <- diag_distros(dataset = dataset)
  expect_matrix(x = res,
                mode = "integer",
                any.missing = FALSE,
                all.missing = FALSE,
                nrows = ncol(dataset),
                ncols = sum(abs(range(dataset))) + 1,
                row.names = "unique",  # this does NOT test the content for correctness, hence below
                col.names = "unique",
                null.ok = FALSE)
  expect_identical(object = rownames(res),
                   expected = colnames(dataset))
  expect_identical(object = colnames(res),
                   expected = as.character(
                     c(min(dataset):max(dataset))))
  expect_integer(x = res,
                 lower = 0,
                 any.missing = FALSE,
                 all.missing = FALSE,
                 null.ok = FALSE)  # this tests for counts
  expect_equivalent(object = res[1, ],
                    expected = diag_distro_max(dataset))
  expect_true(object = diag_same(dataset = dataset),
              info = "diag_same with civicon_2014")

  res <- diag_distro_max(dataset = dataset)
  expect_vector(x = res,
                strict = FALSE,
                any.missing = FALSE,
                all.missing = FALSE,
                names = "unique",
                null.ok = FALSE)
  expect_integer(x = res,
                 lower = 0,
                 any.missing = FALSE,
                 all.missing = FALSE,
                 names = "unique",
                 null.ok = FALSE)
  expect_identical(object = names(res),
                   expected = as.character(
                     c(min(dataset):max(dataset))))

  expect_true(object = diag_inside_grid(dataset = dataset,
                                        grid = diag_distro_max(dataset)),
              info = "diag_inside_grid with civicon_2014")

  expect_true(object = diag_forced(dataset = dataset,
                                   grid = diag_distro_max(dataset)),
              info = "diag_inside_grid with civicon_2014")
})
