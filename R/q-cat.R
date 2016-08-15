#' @title Create Co-Occurence Matrices.
#'
#' @export
#'
#' @description Creates co-occurence matrices from logical q-category assignments.
#'
#' @param ass Named list of logical matrices, one for each participant.
#' Each logical matrix has items as named rows, category indices as columns and logical values in cells.
#'
#' @return
#' An integer array with items as rows and columns, participants as third dimension and cells as co-occurence counts.
#'
#' @details
#' The diagonal is replaced with the \emph{maximum number of categories} for that person, to standardize the entire table.
#'
#' @family import
#'
#' @author Maximilian Held
#'
count_cooccur <- function(ass) {

  # input validation ===
  expect_list(x = ass,
              types = "matrix",
              all.missing = FALSE)
  for (i in names(ass)) {
    expect_matrix(x = ass[[i]],
                  mode = "logical",
                  any.missing = TRUE,
                  all.missing = FALSE,
                  row.names = "unique",
                  null.ok = FALSE,
                  info = paste("Matrix", i, "is not as expected."))
  }

  # body ===
  a <- sapply(X = ass, USE.NAMES = TRUE, simplify = "array", FUN = function(x) {
    m <- tcrossprod(x)
    storage.mode(m) <- "integer"
    diag(m) <- ncol(x)
    return(m)
  })
  names(dimnames(a))[3] <- "people"
  return(a)
}
