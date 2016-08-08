context("Distribution diagnostics")

test_that("work with civicon_2014", {
  data("civicon_2014")
  res <- diag_distros(dataset = civicon_2014$sorts[,,"before"])

  expect_matrix(x = res,
                mode = "integer",
                any.missing = FALSE,
                all.missing = FALSE,
                nrows = ncol(civicon_2014$sorts[,,"before"]),
                ncols = sum(abs(range(civicon_2014$sorts[,,"before"]))) + 1,
                row.names = "unique",  # this does NOT test the content for correctness, hence below
                col.names = "unique",
                null.ok = FALSE)
  expect_identical(object = rownames(res),
                   expected = colnames(civicon_2014$sorts[,,"before"]))
  expect_identical(object = colnames(res),
                   expected = as.character(c(min(civicon_2014$sorts[,,"before"]):max(civicon_2014$sorts[,,"before"]))))
  expect_integer(x = res,
                 lower = 0,
                 any.missing = FALSE,
                 all.missing = FALSE,
                 null.ok = FALSE)  # this tests for counts
  expect_equivalent(object = res[1, ],
                    expected = c(1, 1, 2, 4, 6, 9, 10, 11, 10, 9, 6, 4, 2, 1, 1), )

  expect_true(object = diag_forced(civicon_2014$sorts[,,"before"]))

  res <- diag_distro_max(data = civicon_2014$sorts[,,"before"])
  expect_vector(x = res, strict = FALSE, any.missing = FALSE, all.missing = FALSE, names = "unique", null.ok = FALSE)
  expect_integer(x = res, lower = 0, any.missing = FALSE, all.missing = FALSE, names = "unique", null.ok = FALSE)
  expect_identical(object = names(res),
                   expected = as.character(c(min(civicon_2014$sorts[,,"before"]):max(civicon_2014$sorts[,,"before"]))))
})

