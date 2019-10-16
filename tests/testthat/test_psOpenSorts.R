context("psOpenSorts")

test_that(desc = "construction of multiple open sort list works", code = {
  expect_s3_class(object = los, class = c("psOpenSorts"))
})


context("Import functions for messy open sort data")

test_that(desc = "recreate canonical form", code = {
  # only equivalent, because make_messy function always retains "spurious" "A", "B" names etc.
  expect_equal(
    object = los_from_messy$lisa,
    expected = los$lisa,
    info = "Must be equal, because neither has meaningful names."
  )
  expect_equivalent(
    object = los_from_messy$peter,
    expected = los$peter,
    info = "Must be equivalent (only), because tidy form has meaningful names.")
  expect_equivalent(object = los_from_messy$rebecca, expected = los$rebecca)
  expect_equivalent(object = los_from_messy, expected = los)
})

test_that(desc = "works with komki csvs",
          code = {
  skip("komki renaming broke tests, see https://github.com/maxheld83/pensieve/issues/467")
  komki_path <- "komki_messy"
  if (!dir.exists(paths = komki_path)) {
    komki_path <- file.path("tests", "testthat", "komki_messy")  # for local testing
  }
  desc <- read.csv(file = file.path(komki_path, "cat_desc.csv"),
                   header = TRUE,
                   stringsAsFactors = FALSE,
                   row.names = 1)
  desc <- as.matrix(desc)
  ass <- read.csv(file = file.path(komki_path, "cat_ass.csv"),
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  row.names = 1)
  ass <- as.matrix(ass)
  rownames(ass) <- vctrs::vec_as_names(
    names = rownames(ass),
    repair = "universal"
  )  # old csv has bad names
  canon_cat <- as_psLogicalOpenSorts(logical_open_sorts = ass, descriptions_messy = desc)

  expect_list(x = canon_cat,
              types = c("matrix", "logical"),
              any.missing = FALSE,
              all.missing = FALSE,
              len = ncol(desc),
              names = "strict",
              null.ok = FALSE,
              unique = TRUE)

  # here now come some random tests
  expect_equal(object = colnames(canon_cat$Irene)[canon_cat$Irene["comma", ]],
               expected = c("A", "D", "I"),
               info = "Irene is TRUE for A, D and I.")

  expect_error(object = canon_cat$Justin[, "I"],
               info = "There is no I for Justin, so we expect error.")

  expect_true(object = all(c("C", "H", "D", "E") %in% names(canon_cat$Justin["the_same",])[canon_cat$Justin["the_same",]]),
              info = "Justin has CHDE for `the same`, just in wrong order, expect that it does not matter.")

  expect_equal(object = canon_cat$Knut["i_we",],
               expected = rep(FALSE, 6),
               check.attributes = FALSE,
               info = "Knut never assigned `i_we`, should have false on all 6 described categories.")

  expect_equal(object = canon_cat$Knut["computer", ],
               expected = rep(NA, 6),
               check.attributes = FALSE,
               info = "Knut never saw `computer`, so should have NA on all 6 described categories.")

  expect_equal(object = ncol(canon_cat$Collin),
               expected = 11,
               info = "Collin has used 11 categories, difficult because he actually skipped `J`.")

  expect_true(object = canon_cat$Collin["easter_bunny", 11],
              info = "Collin has used 11 categories, skipped `J` this is his last.")
})
