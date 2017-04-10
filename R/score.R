# CALCULATION ====
#' @title Factor scores.
#'
#' @description Calculates the factor scores as loadings-weighted averages of raw Q-Sorts (regression scores).
#'
#' @export
#'
#' @inheritParams QLoas
#'
#' @inheritParams QSorts
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
  sorts <- QSorts(sorts = sorts, validate = TRUE)

  # Calculation ====
  scores <- matrix(c(1,1), c(2,2))
  rownames(scores) <- c("foo", "bar")

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
QScores <- function(scores, validate = TRUE) {
  # assign class
  scores <- classify_clever(x = scores, classname = "QScores")
  assert_class2(x = scores, validate = validate)
  return(scores)
}


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
                             row.names = "strict")
  #TODO this needs more, such as the range of the raw data etc.

  return(report_checks(res = res, info = "QScores"))
}
