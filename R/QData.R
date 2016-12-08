#' @title Check and make QData.
#'
#' @export
#'
#' @description Checks and makes QData
#'
#' @param presorts An integer matrix, with named rows as item handles, named columns as participant names and cells as presorts.
#' \code{-1L} for `negative`, \code{0L} for `neutral` and \code{1L} for `positive`.
#'
#' @inheritParams QItems
#'
#' @details
#' All data gathered during a Q study.
#'
#' @note
#' \code{presorts} are stored as \code{integer()} because R does not allow factor matrices.
#' Pre-sorting piles are, of course, \emph{categorical} information and should be treated as such.
#'
#' @family import helpers
#' @family validation helpers

QData <- function(presorts = NULL, validate = TRUE) {
  x <- structure(list(presorts = presorts),
                 class = "QData")

  # validation first
  if (validate) {
    expect(x)
  }

  return(x)
}

#' @export
#' @rdname check
check.QData <- function(x) {
  # Input validation ====
  res <- NULL  # appease R

  # check names
  res$list <- check_list(x = x, names = "strict", len = 1)
  res$names <- check_subset(x = names(x), choices = c("presorts"))

  presorts <- x$presorts # makes below code easier to read

  # check presorts
  res$presorts <- check_matrix(x = presorts,
                               mode = "integer",
                               any.missing = TRUE,
                               all.missing = FALSE,
                               null.ok = TRUE)
  # return ====
  # preliminary step is necessary b/c res is list and may contain logical AND character strings
  all_res <- sapply(X = res, FUN = function(x) {isTRUE(x)})  # now this is always a logical vector
  if (all(all_res)) {
    return(TRUE)  # everyone is happy!
  } else {
    return(res[!all_res][[1]])  # let's just take the first error, one at a time
  }
}
