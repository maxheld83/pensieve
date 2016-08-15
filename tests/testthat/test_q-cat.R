context("Co-occurence matrix generation")

test_that(desc = "works with komki",
          code = {
  data("komki")
  cooc <- count_cooccur(ass = komki$qcat$ass)
  expect_array(x = cooc,
               mode = "integer",
               any.missing = TRUE,
               d = 3,
               null.ok = FALSE)
  expect_integer(x = cooc,
                 lower = -1)
  expect_equal(object = cooc["headturner", "the-better", "Valentine"],
               expected = 1,
               info = "For `Valentine` `headturner` and `the-better` do not overlap 1 times.")
  expect_equal(object = cooc["riddle", "eating-grandpa", "Jalena"],
               expected = 0,
               info = "For `Jalena`, `riddle` and `eating-grandpa` do not overlap 0 times.")
  expect_scalar_na(x = cooc["computer", "riddle", "Nhome"],
                   null.ok = FALSE,
                   info = "`Nhome` never saw `computer`, so `computer` and, say, `riddle` must be NA.")
  expect_scalar_na(x = cooc["computer", "black-white", "Nhome"],
                   null.ok = FALSE,
                   info = "`Nhome` never saw `computer` or `black-white`, the two must be NA.")
  expect_equal(object = unique(diag(cooc[,,"Thomas"])),
               expected = 12,
               info = "`Thomas` assigned and/or described 12 categories, so the diagonal should always be 12.")
  expect_equal(object = dim(cooc[,,"Thomas"]),
               expected = c(41, 41),
               info = "`Thomas` as everyone else had a maximum of 41 items, so that must be the extend of his matrix.")
})
