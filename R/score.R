# CALCULATION ====
#' @title Factor scores.
#'
#' @description Calculates the factor scores as loadings-weighted averages of raw Q-Sorts (regression scores).
#'
#' @export
#'
#' @inheritParams QLoas
#'
#' @inheritParams psClosedSorts
#'
#' @family analysis functions
#' @family scoring functions
#'
#' @examples
#' sorts <- civicon_2014$qData$sorts[,,"before"]  # preparatory step
#' cors <- correlate(sorts = sorts)  # preparatory step
#' loas <- extract(cors = cors, nfactors = 3, fa_type = "pca")  # preparatory step
#' scores <- score(loas = loas, sorts = sorts)
score <- function(loas, sorts) {
  # Input validation ====
  loas <- QLoas(loas = loas, validate = TRUE)
  sorts <- psClosedSorts(sorts = sorts, validate = TRUE)

  # Calculation ====
  scores <- apply(X = loas, MARGIN = 2, FUN = function(x) {
    allweighted <- sweep(x = sorts, MARGIN = 2, STATS = x, FUN = "*")
    scores <- apply(X = allweighted, MARGIN = 1, FUN = function(x) {
      mean(x, na.rm = TRUE)
    })
    scores <- scale(scores)
    return(scores)
  })
  colnames(scores) <- colnames(loas)
  rownames(scores) <- rownames(sorts)

  # Return ====
  scores <- QScores(scores = scores, validate = TRUE)
  return(scores)
}

# CLASS CONSTRUCTION ====
#' @rdname score
#'
#' @export
#'
#' @template construct
#'
#' @param scores A numerical matrix with factor scores.
#'
#' @examples
#' # this just assigns the class, without validation (not recommended)
#' scores <- QScores(scores = scores, validate = FALSE)
QScores <- produce_class_constructor(classname = "QScores", fun = function(scores) {
  return(scores)
})


#' @describeIn score validation
#'
#' @export
#'
#' @template check
#'
#' @examples
#' # this validates the class
#' check(x = scores)
check.QScores <- function(x) {
  res <- NULL

  res$matrix <- check_matrix(x = x,
                             mode = "numeric",
                             any.missing = TRUE,
                             all.missing = FALSE,
                             row.names = "unique")
  #TODO this needs more, such as the range of the raw data etc.

  return(report_checks(res = res, info = "QScores"))
}
