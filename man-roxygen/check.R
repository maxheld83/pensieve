#' @description Check S3 class.
#'
#' @param x class object created by respective constructor function.
#'
#' @family validation functions
#'
#' @details
#' `check()`, `test()`, `assert()` and `expect()` are extensions the family of functions from the [checkmate](https://github.com/mllg/checkmate) package.
#' `check()` returns `TRUE` or the error message, `assert()` only returns a message in case of an error, `test()` returns `TRUE` or `FALSE`.
#' `expect()` is for internal use with testing via [`testthat`](https://github.com/hadley/testthat)).
#' `need()` returns `NULL` or the error message and is for interal use with the accio web frontend inside [shiny::validate()].
#'
#' @examples
#' # create checkable object
#' x <- QItemConcourse(
#'   concourse = matrix(
#'     data = c(
#'       "Man lives to work.", "Man lebt, um zu arbeiten.",
#'       "Man works to live.", "Man arbeitet, um zu leben."
#'     ),
#'     nrow = 2, ncol = 2,
#'     dimnames = list(
#'       items = c("live_2_work", "work_2_live"),
#'       languages = c("english", "ngerman")  # ideally, these are valid babel languages
#'     )
#'   )
#' )
#'
#' # check object and friends ...
#' check(x)  # returns TRUE or error message
#' test(x)  # returns TRUE or FALSE
#' assert(x)  # returns error or silently object
#' need(x)  # returns NULL or error message
