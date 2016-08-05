context("Distribution helpers")

test_that("count_distros works with civicon_2014", {
  data("civicon_2014")
  res <- count_distros(civicon_2014$sorts[,,"before"])
  expect_list(x = res,
              types = "list",
              any.missing = TRUE,
              all.missing = FALSE,
              len = ncol(civicon_2014$sorts[,,"before"]))
  expect_equal(object = res[[1]]$x, expected = c(-7:7))
  expect_equal(object = res[[1]]$freq, expected = c(1, 1, 2, 4, 6, 9, 10, 11, 10, 9, 6, 4, 2, 1, 1))
  expect_true(object = is_forced(civicon_2014$sorts[,,"before"]))
})

