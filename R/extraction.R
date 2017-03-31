# CALCULATION ====
#' Dimensionality reduction.
#'
#' @description Wraps Principal Components Analysis (PCA) to extract component loadings.
#'
#' @export
#'
#' @inheritParams QCors
#'
#' @param nfactors
#' Positive, non-zero integer scalar, giving the number of components (or factors) to retain.
#' Must be smaller than the number of observations (here: items) on which `cors` are based.
#'
#' @param fa_type
#' String giving the type of factor analysis performed.
#' Currently only `"pca"` for principal components analysis (PCA) is allowed.
#'
#' @family analysis functions
#' @family extraction functions
#'
#' @examples
#' # this just runs the calculations, but also classes and validates the results
#' cors <- correlate(sorts = civicon_2014$qData$sorts[,,"before"])  # preliminary step
#' loas <- extract(cors = cors, nfactors = 3, fa_type = "pca")
extract <- function(cors, nfactors, fa_type = "pca") {
  # Input validation ====
  cors <- QCors(cors = cors, validate = TRUE)

  assert_int(x = nfactors, na.ok = FALSE, lower = 0, null.ok = FALSE)

  assert_choice(x = fa_type, choices = c("pca"))


  # Calculation ====
  # this is just a very thin wrapper for now
  loas <- unclass(stats::princomp(covmat = cors, scores = FALSE)$loadings)[,1:nfactors]
  colnames(loas) <- NULL  # there are no meaningful names for now

  return(loas)
}


#' CLASS CONSTRUCTION ====
#' #' @title Check and make Q Loadings.
#' #'
#' #' @export
#' #'
#' #' @description Checks and makes Q Loadings.
#' #'
#' #' @param qloa A numerical matrix with people-variables as rows, q factors as columns and loadings in cells.
#' #' Loadings must be between `-1` and `1`.
#' #'
#' #' @template construct
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
