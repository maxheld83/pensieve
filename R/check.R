#' @title check: validate S3 classes
#' @description Validate S3 classes from pensieve package
#' @export
#' @param x class object created by respective \code{make_} function.
check <- function(x) {
  UseMethod("check")
}
