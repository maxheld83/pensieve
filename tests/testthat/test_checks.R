context("Type validation")

good_obj <- QItemConcourse(
  concourse = matrix(
    data = c(
      "Man lives to work.", "Man lebt, um zu arbeiten.",
      "Man works to live.", "Man arbeitet, um zu leben."
    ),
    nrow = 2, ncol = 2,
    dimnames = list(
      items = c("live_2_work", "work_2_live"),
      languages = c("english", "ngerman")  # ideally, these are valid babel languages
    )
  ),
  validate = FALSE
)

bad_obj <- good_obj
rownames(bad_obj) <- c("live-2-work", "work-2-live")  # this should FAIL because not valid r names

test_that(desc = "check on good object returns TRUE", code = {
  testthat::expect_true(object = pensieve::check(good_obj))
})

test_that(desc = "check on bad object returns string", code = {
  checkmate::expect_string(x = pensieve::check(bad_obj),
                           null.ok = FALSE)
})

test_that(desc = "test on good object returns TRUE", code = {
  testthat::expect_true(object = pensieve::test(good_obj))
})

test_that(desc = "test on bad object returns FALSE", code = {
  testthat::expect_false(object = pensieve::test(bad_obj))
})

test_that(desc = "assert on good object silently returns object", code = {
  testthat::expect_silent(object = pensieve::assert(good_obj))
})

test_that(desc = "assert on bad object errors out with good error message", code = {
  testthat::expect_error(object = pensieve::assert(bad_obj), regexp = "Rows must be named according")
})

test_that(desc = "expect on good objects returns expectation", code = {
  testthat::expect_success(expr = pensieve::expect(good_obj))
})

test_that(desc = "expect on bad object returns failure", code = {
  testthat::expect_failure(expr = pensieve::expect(bad_obj))
})

test_that(desc = "need on good object returns null", code = {
  testthat::expect_null(object = pensieve::need(good_obj))
})

test_that(desc = "need on bad object returns string", code = {
  checkmate::expect_string(x = pensieve::need(bad_obj))
})
