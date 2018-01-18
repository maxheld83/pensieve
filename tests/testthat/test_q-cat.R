context("Co-occurence matrix generation")

test_that(desc = "works with komki",
          code = {
  data("komki")
  cooc <- count_cooccur(ass = komki$osorts)
  expect_array(x = cooc,
               mode = "integer",
               any.missing = TRUE,
               d = 3,
               null.ok = FALSE)
  expect_integer(x = cooc,
                 lower = -1)
  expect_equal(object = cooc["kopf_verdrehen", "je_desto", "Valentine"],
               expected = 1,
               info = "For `Valentine` `kopf_verdrehen` and `je_desto` do not overlap 1 times.")
  expect_equal(object = cooc["nuschel", "opa_essen", "Jalena"],
               expected = 0,
               info = "For `Jalena`, `nuschel` and `opa_essen` do not overlap 0 times.")
  # these tests no longer work, b/c only 35 items are in current osorts
  # expect_scalar_na(x = cooc["computer", "nuschel", "Nhome"],
  #                  null.ok = FALSE,
  #                  info = "`Nhome` never saw `computer`, so `computer` and, say, `nuschel` must be NA.")
  # expect_scalar_na(x = cooc["computer", "schwarz_weiss", "Nhome"],
  #                  null.ok = FALSE,
  #                  info = "`Nhome` never saw `computer` or `schwarz_weiss`, the two must be NA.")
  # expect_equal(object = unique(diag(cooc[,,"Thomas"])),
  #              expected = 12,
  #              info = "`Thomas` assigned and/or described 12 categories, so the diagonal should always be 12.")
  expect_equal(object = dim(cooc[,,"Thomas"]),
               expected = c(35, 35),
               info = "`Thomas` as everyone else had a maximum of 35 items, so that must be the extend of his matrix.")
})
