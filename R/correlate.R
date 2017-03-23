# CLASS CONSTRUCTION ====
#' @title Check and make Q correlations.
#'
#' @description Check and make Q correlations.
#'
#' @export
#'
#' @param cors A numerical matrix with correlations.
#'
#' @template validate
#'
#' @return
#' A numerical matrix with people-variables as rows and columns, correlation coefficients in cells.
#' Class [`QCors`][QCors].
#'
#' @family correlation functions
QCors <- function(cors, validate = TRUE) {
  # assign class
  cors <- classify_clever(x = cors, classname = "QCors")
  assert_class2(x = cors, validate = validate)
  return(cors)
}

#' @export
#' @rdname check
check.QCors <- function(x) {
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
                             lower = -1L,
                             upper = 1L)
  res$diag <- check_integerish(x = diag(x),
                               any.missing = FALSE,
                               all.missing = FALSE,
                               lower = 1L - sqrt(.Machine$double.eps),
                               upper = 1L + sqrt(.Machine$double.eps))  # just some double/integer roundind errors

  return(report_checks(res = res, info = "QCors"))
}

#' @title Correlate Q Sorts.
#'
#' @description Correlate Q Sorts.
#'
#' @export
#'
#' @inherit QSorts params
#'
#' @inherit stats::cor params
#'
#' @inherit QCors return
#'
#' @family analysis functions
#' @family correlation functions
#'
#' @examples
#' correlate(sorts = civicon_2014$qData$sorts[,,"before"])
#'
correlate <- function(sorts, method = "pearson", use = "pairwise.complete.obs") {
  # input validation ====
  assert_choice(x = method,
                choices = c("spearman", "kendall", "pearson"))
  assert_choice(x = use,
                choices = c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"))
  sorts <- QSorts(sorts = sorts, validate = TRUE)  # assigns class and validates whether ok

  # for now, this only deals with 2d objects, so we have to test that too
  # in general, sorts can be n-dim
  assert_matrix(x = sorts,
                mode = "integerish",
                any.missing = TRUE,
                all.missing = TRUE)

  # run correlations ====
  cors <- stats::cor(x = sorts, use = use, method = method)

  # assign classes
  cors <- QCors(cors = cors, validate = TRUE)
  cors <- classify_clever(x = cors, classname = paste0("QCors", capitalize(method))) # method specific stuff

  # return ====
  return(cors)
}

#' @title Plot Q correlations.
#'
#' @description Produces a heatmap from a given correlation matrix.
#'
#' @param x An object of class [`QCors`][QCors].
#'
#' @inherit QCors params
#'
#' @template plot
#'
#' @family correlation functions

plot.QCors <- function(x, use_js = NULL) {
  # Input validation ====
  assert_flag(x = use_js,
              na.ok = FALSE,
              null.ok = TRUE)

  x <- QCors(cors = x, validate = TRUE)  # make and validate QCorr first

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
