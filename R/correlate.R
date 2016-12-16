#' @title Correlate Q Sorts.
#'
#' @export
#'
#' @description Correlate Q Sorts.
#'
#' @param sorts an integer matrix with item-cases as named rows, people-variables as named columns and item positions in cells.
#' Higher-dimensionality objects fom \code{\link{QSorts}} must be subset.
#'
#' @param algorithm choice from \code{'pearson'}, \code{'spearman'} or \code{'kendall'}, passed on to \code{\link[stats]{cor}}.
#' Defaults to \code{'spearman'}.
#'
#' @inheritParams stats::cor
#'
#' @family analysis
#'
#' @examples
#' correlate(sorts = civicon_2014$qData$sorts[,,"before"])
#'
correlate <- function(sorts, algorithm = "spearman", use = "pairwise.complete.obs") {
  # input validation ====
  assert_choice(x = algorithm,
                choices = c("spearman", "kendall", "pearson"))
  assert_choice(x = use,
                choices = c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"))
  sorts <- QSorts(qsorts = sorts, validate = TRUE)  # assigns class and validates whether ok

  # correlations ====
  cormatrix <- stats::cor(x = sorts, use = use, method = algorithm)

  # return ====
  class(cormatrix) <- c(class(cormatrix),  # use whichever it already has, just in case
                        "QCorr",  # q specific stuff
                        paste0("QCorr", capitalize(algorithm)))  # method specific stuff
  #TODO perhaps it would be better to outsource the class marking for algorithm to where the algorithm actually runs
  return(cormatrix)
}


#' @export
#' @rdname check
check.QCorr <- function(x) {
  res <- NULL

  res$matrix <- check_matrix(x = x,
                             mode = "numeric",
                             any.missing = TRUE,
                             all.missing = FALSE,
                             row.names = "strict",
                             col.names = "strict")
  res$range <- check_numeric(x = x,
                             finite = TRUE,
                             any.missing = TRUE,
                             all.missing = FALSE,
                             lower = -1,
                             upper = 1)

  res$diag <- check_integerish(x = diag(x),
                               lower = 1,
                               upper = 1)

  return(report_checks(res = res, info = "QCorr"))
}
