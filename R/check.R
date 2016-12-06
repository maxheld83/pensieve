#' @title Validate S3 classes from this package.
#' @description Use \code{check}, \code{test}, \code{assert} and \code{expect} to validate S3 classed objects from this package.
#' @export
#' @param x class object created by respective constructor function.
#' @inheritParams checkmate::makeAssertion
#' @inheritParams checkmate::makeExpectation
#' @family import helpers
#' @family validation functions
#' @examples
#' check(civicon_2014$QItems)
check <- function(x) {
  UseMethod("check")
}

#' @rdname check
test <- function(x) {
  UseMethod("test")
}

#' @rdname check
test.default <- function(x) {
  res <- check(x)
  return(makeTest(res = res))
}

#' @rdname check
expect <- function(x, info = NULL, label = vname(x)) {
  UseMethod("expect")
}

#' @rdname check
expect.default <- function(x, info = NULL, label = vname(x)) {
  res <- check(x)
  return(makeExpectation(x = x, res = res, info = info, label = label))
}

#' @rdname check
assert <- function(x, collection = NULL, var.name = vname(x)) {
  UseMethod("assert")
}

#' @rdname check
assert.default <- function(x, collection = NULL, var.name = vname(x)) {
  res <- check(x)
  return(makeAssertion(x = x, res = res, var.name = var.name, collection = collection))
}
