#' @title Validate S3 classes from this package.
#' @description Use \code{check}, \code{test}, \code{assert} and \code{expect} to validate S3 classed objects from this package.
#' @export
#' @param x class object created by respective \code{make_} function.
#' @family import helpers
#' @family validation functions
check <- function(x) {
  UseMethod("check")
}

#' @rdname check
expect <- function(x) {
  UseMethod("expect")
}

#' @rdname check
test <- function(x) {
  UseMethod("test")
}

#' @rdname check
assert <- function(x) {
  UseMethod("assert")
}
