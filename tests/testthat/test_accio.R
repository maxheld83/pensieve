context("'accio' web frontend")

test_that(desc = "accio is not available on CRAN release", code = {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    skip("This test is not run on CRAN, so accio can be exposed.")
  }
  expect_false(object = checkmate::test_directory_exists(x = pensieve:::accio_path),
               info = "This test may run on CRAN, so accio MUST not be exposed.")
})
