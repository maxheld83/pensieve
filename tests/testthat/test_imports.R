context("Import functions for Q-Cat")

test_that(desc = "works with komki csvs",
          code = {
  desc <- read.csv(file = system.file("extdata",
                                      "import_example/q_cat/komki/cat_desc.csv",
                                      package = "pensieve"),
                   header = TRUE,
                   stringsAsFactors = FALSE,
                   row.names = 1)
  desc <- as.matrix(desc)
  ass <- read.csv(file = system.file("extdata",
                                     "import_example/q_cat/komki/cat_ass.csv",
                                     package = "pensieve"),
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  row.names = 1)
  ass <- as.matrix(ass)
  canon_cat <- import_qcat(desc = desc, ass = ass)

  expect_list(x = canon_cat,
              types = c("matrix", "logical"),
              any.missing = FALSE,
              all.missing = FALSE,
              len = ncol(desc),
              names = "strict",
              null.ok = FALSE,
              unique = TRUE)

  # now we add the letters again, which is bad,
  # only doing that to make manual testing easier

  canon_cat <- sapply(X = canon_cat,
                      FUN = function(x) {
                        colnames(x) <- LETTERS[1:ncol(x)]
                        return(x)
                      })

  # here now come some random tests
  expect_equal(object = colnames(canon_cat$Irene)[canon_cat$Irene["comma", ]],
               expected = c("A", "D", "I"),
               info = "Irene is TRUE for A, D and I.")

  expect_error(object = canon_cat$Justin[, "I"],
               info = "There is no I for Justin, so we expect error.")

  expect_true(object = all(c("C", "H", "D", "E") %in% names(canon_cat$Justin["the-same",])[canon_cat$Justin["the-same",]]),
              info = "Justin has CHDE for `the same`, just in wrong order, expect that it does not matter.")

  expect_equal(object = canon_cat$Knut["i-we",],
               expected = rep(FALSE, 6),
               check.attributes = FALSE,
               info = "Knut never assigned `i-we`, should have false on all 6 described categories.")

  expect_equal(object = canon_cat$Knut["computer", ],
               expected = rep(NA, 6),
               check.attributes = FALSE,
               info = "Knut never saw `computer`, so should have NA on all 6 described categories.")

  expect_equal(object = ncol(canon_cat$Collin),
               expected = 11,
               info = "Collin has used 11 categories, difficult because he actually skipped `J`.")

  expect_true(object = canon_cat$Collin["easter-bunny", 11],
              info = "Collin has used 11 categories, skipped `J` this is his last.")

})
