#' @title Correlate Q Sorts.
#'
#' @aliases QCorr
#'
#' @export
#'
#' @description Correlate Q Sorts.
#'
#' @param sorts an integer matrix with item-cases as named rows, people-variables as named columns and item positions in cells.
#' Higher-dimensionality objects fom [`QSorts`][QSorts] must be subset.
#'
#' @param algorithm choice from `'pearson'`, `'spearman'` or `'kendall'`, passed on to [stats::cor()].
#' Defaults to `'spearman'`.
#'
#' @inheritParams stats::cor
#'
#' @return A numerical matrix of class [`QCorr`][QCorr].
#'
#' @family analysis functions
#' @family correlation functions
#'
#' @examples
#' correlate(sorts = civicon_2014$qData$sorts[,,"before"])
#'
correlate <- QCorr <- function(sorts, algorithm = "spearman", use = "pairwise.complete.obs") {
  # input validation ====
  assert_choice(x = algorithm,
                choices = c("spearman", "kendall", "pearson"))
  assert_choice(x = use,
                choices = c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"))
  sorts <- QSorts(qsorts = sorts, validate = TRUE)  # assigns class and validates whether ok

  # run correlations ====
  cormatrix <- stats::cor(x = sorts, use = use, method = algorithm)

  # assign classes
  cormatrix <- classify_clever(x = cormatrix, classname = "QCorr") # q specific stuff
  cormatrix <- classify_clever(x = cormatrix, classname = paste0("QCorr", capitalize(algorithm))) # method specific stuff

  # return ====
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


#' @title Plot Q correlations.
#'
#' @description Produces a heatmap from a given correlation matrix.
#'
#' @param x numerical matrix with correlation coefficients or object of class [`QCorr`][QCorr], as created by [correlate()].
#'
#' @param use_js Logical flag, indicating whether an interactive, javascript-based version of the plot should be returned.
#' Suiteable for HTML output (via [rmarkdown::render()] and friends) and the RStudio IDE.
#' Defaults to `NULL`, in which case the appropriate setting is inferred at runtime.
#'
#' @param ... other parameters to be passed through to plotting functions.
#'
#' @return Returns a plot of the correlation matrix as a list of class [`ggplot`][ggplot].
#'
#' @export
#'
#' @family correlation functions
#' @family plotting functions
plot.QCorr <- function(x, use_js = NULL, ...) {
  # Input validation ====
  assert_flag(x = use_js,
              na.ok = FALSE,
              null.ok = TRUE)

  x <- classify_clever(x = x, classname = "QCorr")  # gotta make sure it IS QCorr in the first place
  assert(x)

  # Data preparation ====
  m <- reshape2::melt(data = x, value.name = "Correlation", varnames = c("x", "y"))
  x <- y <- Correlation <- NULL  # hack job to appease R CMD CHECK as per http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

  # let's extract the appropriate string for chosen method from classname
  if ("QCorrSpearman" %in% class(x)) {
    cormethod <- "Spearman"
  } else if ("QCorrPearson" %in% class(x)) {
    cormethod <- "Pearson"
  } else if ("QCorrKendall" %in% class(x)) {
    cormethod <- "Kendall"
  } else {
    cormethod <- NULL
  }

  # let's set a good legend
  ltitle <- if (is.null(cormethod)) {
    paste0("Correlation \n Coefficient")
  } else {
    paste0(cormethod, "'s ", "\n Correlation \n Coefficient")
  }

  # Plotting ====
  g <- ggplot(data = m, mapping = aes(x = x, y = y, fill = Correlation, label = round(x = Correlation, digits = 1)))
  g <- g + geom_tile()
  g <- g + scale_fill_gradient2(low = "red",
                                high = "blue",
                                mid = "white",
                                limits = c(-1, 1),  # make sure whole range of values is covered
                                name = ltitle)
  g <- g + geom_text()
  g <- g + theme(axis.title = element_blank())  # kill axis labels
  g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))  # rotate x axis labels


  # Make interactive ====
  if (is.null(use_js)) {
    use_js <- is_use_js()
  }
  if (use_js) {
    g <- plotly::ggplotly(p = g)
  }
  return(g)
}
