# CALCULATION ====
#' @title Dimensionality reduction.
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

# CLASS CONSTRUCTION ====
#' @rdname extract
#'
#' @export
#'
#' @template construct
#'
#' @param loas A numerical matrix with loadings.
#'
#' @examples
#' # this just assigns the class, without validation (not recommended)
#' loas <- QLoas(loas = loas, validate = FALSE)
QLoas <- function(loas, validate = TRUE) {
  # assign class
  loas <- classify_clever(x = loas, classname = "QLoas")
  assert_class2(x = loas, validate = validate)
  return(loas)
}


#' @describeIn extract validation
#'
#' @export
#'
#' @template check
#'
#' @examples
#' # this validates the class
#' check(loas)
check.QLoas <- function(x) {
  res <- NULL

  res$matrix <- check_matrix(x = x,
                             mode = "numeric",
                             any.missing = TRUE,
                             all.missing = FALSE,
                             row.names = "strict")
  res$range <- check_numeric(x = x,
                             finite = TRUE,
                             any.missing = TRUE,
                             all.missing = FALSE,
                             lower = -1L,
                             upper = 1L)

  return(report_checks(res = res, info = "QLoas"))
}

