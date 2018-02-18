context(desc = "type validation")
test_that(desc = "validate errors out on unknown class", code = {
  expect_error(object = validate_S3(x = "foo"))
  expect_error(object = validate_S3(x = NULL))
})
test_that(desc = "validate returns 'NULL' or character vector", code = {
  expect_null(object = validate_S3(x = good_obj))
  expect_character(
    x = validate_S3(x = bad_obj),
    min.chars = 1,
    any.missing = FALSE,
    all.missing = FALSE,
    null.ok = FALSE,
    len = 2
  )
})
test_that(desc = "check returns 'TRUE' or character string", code = {
  expect_true(
    object = check_S3(x = good_obj)
  )
  expect_character(
    x = check_S3(x = bad_obj),
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
})
test_that(desc = "test returns 'TRUE' or 'FALSE'", code = {
  expect_true(object = test_S3(x = good_obj))
  expect_false(object = test_S3(x = bad_obj))
})
test_that(desc = "expect returns expectation", code = {
  expect_success(expr = expect_S3(x = good_obj))
  expect_failure(expr = expect_S3(x = bad_obj))
})
test_that(desc = "assert returns 'x' invisibly or error", code = {
  expect_silent(object = assert_S3(x = good_obj))
  expect_error(object = assert_S3(x = bad_obj))
})
test_that(desc = "need returns 'NULL' or character string", code = {
  expect_null(object = need_S3(x = good_obj))
  expect_character(
    x = need_S3(x = bad_obj),
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
})

# context("Type validation")
#
# good_obj <- ItemConcourse(
#   concourse = matrix(
#     data = c(
#       "Man lives to work.", "Man lebt, um zu arbeiten.",
#       "Man works to live.", "Man arbeitet, um zu leben."
#     ),
#     nrow = 2, ncol = 2,
#     dimnames = list(
#       items = c("live_2_work", "work_2_live"),
#       languages = c("english", "ngerman")  # ideally, these are valid babel languages
#     )
#   ),
#   validate = FALSE
# )
#
# bad_obj <- good_obj
# rownames(bad_obj) <- c("live-2-work", "work-2-live")  # this should FAIL because not valid r names
#
# test_that(desc = "check on good object returns TRUE", code = {
#   testthat::expect_true(object = pensieve::check(good_obj))
# })
#
# test_that(desc = "check on bad object returns string", code = {
#   checkmate::expect_string(x = pensieve::check(bad_obj),
#                            null.ok = FALSE)
# })
#
# test_that(desc = "test on good object returns TRUE", code = {
#   testthat::expect_true(object = pensieve::test(good_obj))
# })
#
# test_that(desc = "test on bad object returns FALSE", code = {
#   testthat::expect_false(object = pensieve::test(bad_obj))
# })
#
# test_that(desc = "assert on good object silently returns object", code = {
#   testthat::expect_silent(object = pensieve::assert(good_obj))
# })
#
# test_that(desc = "assert on bad object errors out with good error message", code = {
#   testthat::expect_error(object = pensieve::assert(bad_obj), regexp = "Rows must be named according")
# })
#
# test_that(desc = "expect on good objects returns expectation", code = {
#   testthat::expect_success(expr = pensieve::expect(good_obj))
# })
#
# test_that(desc = "expect on bad object returns failure", code = {
#   testthat::expect_failure(expr = pensieve::expect(bad_obj))
# })
#
# test_that(desc = "need on good object returns null", code = {
#   testthat::expect_null(object = pensieve::need(good_obj))
# })
#
# test_that(desc = "need on bad object returns string", code = {
#   checkmate::expect_string(x = pensieve::need(bad_obj))
# })
