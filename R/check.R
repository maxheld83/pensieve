#' @title Validate S3 classes from this package.
#' @description Use \code{check} to validate S3 classed objects from this package.
#' @export
#' @param x [any] class object created by respective \code{make_} function.
#' @family import helpers
#' @family validation functions
check <- function(x) {
  UseMethod("check")
}
