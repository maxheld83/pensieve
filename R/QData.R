#' @title Check and make QPreSorts
#'
#' @export
#'
#' @description Checks and makes QPreSorts
#'
#' @param presorts An integer matrix, with named rows as item handles, named columns as participant names and cells as presorts.
#' \code{-1L} for `negative`, \code{0L} for `neutral` and \code{1L} for `positive`.
#'
#' @inheritParams QItems
#'
#' @note
#' \code{presorts} are stored as \code{integer()} because R does not allow factor matrices.
#' Pre-sorting piles are, of course, \emph{categorical} information and should be treated as such.
#'
#' @family import helpers
#' @family validation helpers

QPreSorts <- function(presorts, validate = TRUE) {
  assert_flag(x = validate,
              na.ok = FALSE,
              null.ok = FALSE)

  class(presorts) <- "QPreSorts"

  # validation first
  if (validate) {
    assert(presorts)
  }
  return(presorts)
}

#' @export
#' @rdname check
check.QPreSorts <- function(x) {
  res <- NULL  # appease R

  res$matrix <- check_matrix(x = x,
                             mode = "integer",
                             any.missing = TRUE,
                             all.missing = FALSE,
                             row.names = "strict",
                             col.names = "strict")
  res$range <- check_integer(x = x,
                             any.missing = TRUE,
                             lower = -2,
                             upper = 2)

  return(report_checks(res = res, info = "QPreSorts"))
}
