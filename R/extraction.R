#' # CLASS CONSTRUCTION ====
#' #' @title Check and make Q Loadings.
#' #'
#' #' @export
#' #'
#' #' @description Checks and makes Q Loadings.
#' #'
#' #' @param qloa A numerical matrix with people-variables as rows, q factors as columns and loadings in cells.
#' #' Loadings must be between `-1` and `1`.
#' #'
#' #' @template validate
#' #'
#' #' @family
#' QLoa <- function(loa, validate = TRUE) {
#'   return(NULL)
#' }
#'
#' #' @export
#' #' @rdname check
#' check.QLoa <- function(x) {
#'   res <- NULL
#'
#'   res$matrix <- check_matrix(x = x,
#'                              mode = "numeric")
#' }
#'
#' # WRAPPER ====
#' #' @title Extract Q Factors.
#' #'
#' #' @aliases QLoa
#' #'
#' #' @export
#' #'
#' #' @description Extract Q Factors.
#' #'
#' #' @inherit Qsorts qsorts
#' #'
#' #' @param nfactors A single integer, specifying how many factors should be extracted.
#' #'
#' #' @param corr A numerical matrix of class [`QCorr`][QCorr], or which can be thus coerced.
#' #' Defaults to `NULL`, in which case correlation matrix is calculated via [`correlate`][correlate].
#' #'
#' #' @return A numerical matrix of class [`QLoa`][QLoa].
#' #'
#' #' @family extraction functions
#' #'
#' #' @examples
#' #' # 1+1  # still missing
#'
#' extract <- function(qsorts, nfactors, corr = NULL) {
#'   return(2)
#' }
#'
#' # PCA ====
