# CALCULATION ====
#' @title Correlate Q Sorts.
#'
#' @description Wraps `stats::cor` to calculate correlation coefficients.
#'
#' @export
#'
#' @inherit psClosedSorts params
#'
#' @param method
#' A character string indicating which correlation coeffocient to use.
#' Must be `"pearson"`, `"kendall"` or `"spearman"` (recommended default).
#'
#' @param use
#' A character string giving a method for computing covariances in the presence of missing data.
#' Must be `"everything"`, `"all.obs"`,, `"complete.obs"`, `"na.or.complete"` or `"pairwise.complete.obs"` (recommended default).
#' For more information, see [stats::cor()]
#'
#' @family analysis functions
#' @family correlation functions
#'
#' @examples
#' # this just runs the calculations, but also classes and validates the results
#' cors <- correlate(sorts = civicon_2014$qData$sorts[,,"before"])
correlate <- function(sorts, method = "spearman", use = "pairwise.complete.obs") {
  # input validation ====
  assert_choice(x = method,
                choices = c("spearman", "kendall", "pearson"))
  assert_choice(x = use,
                choices = c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"))
  sorts <- psClosedSorts(sorts = sorts, validate = TRUE)  # assigns class and validates whether ok

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


# CLASS CONSTRUCTION ====
#' @rdname correlate
#'
#' @export
#'
#' @template construct
#'
#' @param cors A numerical matrix with correlations.
#'
#' @examples
#' # this just assigns the class, without validation (not recommended)
#' cors <- QCors(cors = cors, validate = FALSE)
QCors <- produce_class_constructor(classname = "QCors", fun = function(cors) {
  return(cors)
})

#' @describeIn correlate validation
#'
#' @export
#'
#' @template check
#'
#' @examples
#' # this validates the class
#' check(cors)
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


# PLOTTING ====
#' @describeIn correlate plotting
#'
#' @export
#'
#' @template plot
#'
#' @param n_obs
#' Integer scalar, giving the number of observations (here: items), on which the correlations are based.
#' Defaults to `NULL`.
#' If given, `type = density` includes a density estimate for Pearson's $r$ in random data.
#'
#' @param summarize
#' A logical flag, indicating whether the object should be summarized before plotting.
#' If `TRUE`, a plot with a density estimate of individual data points is returned, suitable for medium to large objects (numbers of people-variables).
#' If `FALSE`, a plot with individual data points is returned, suitable for small to medium-sized objects (numbers of people-variables).
#'
#' Defaults to `NULL`, in which case the appropriate plot is inferred from the size.
#'
#' @examples
#' # makes a heatmap
#' plot(x = cors, use_js = NULL, type = "heatmap", n_obs = NULL)
#'
#' # makes density estimate
#' plot(x = cors, use_js = NULL, type = "density", n_obs = nrow(civicon_2014$qData$sorts[,,"before"]))
#'
plot.QCors <- function(x, summarize = NULL, n_obs = NULL, use_js = NULL, ...) {
  # Input validation ====
  cors <- QCors(cors = x, validate = TRUE)  # make and validate QCors first

  use_js <- assert_n_infer_use_js(use_js = use_js)

  summarize <- assert_n_infer_summarize(summarize = summarize, x = x)

  # check n_obs
  assert_scalar(x = n_obs,
                na.ok = FALSE,
                null.ok = TRUE)
  assert_integerish(x = n_obs,
                    len = 1,
                    null.ok = TRUE)


  # let's extract the appropriate string for chosen method from classname
  if ("QCorsSpearman" %in% class(cors)) {
    method <- "Spearman"
  } else if ("QCorsPearson" %in% class(cors)) {
    method <- "Pearson"
  } else if ("QCorsKendall" %in% class(cors)) {
    method <- "Kendall"
  } else {
    method <- NULL
  }
  # let's set a good legend
  correlation_title <- if (is.null(method)) {
    paste0("Correlation Coefficient")
  } else {
    paste0(method, "'s ", "Correlation Coefficient")
  }

  # Plotting ====
  if (summarize) {
    upper <- unclass(cors)[upper.tri(unclass(cors))]
    df <- reshape2::melt(data = upper)

    if (!is.null(n_obs)) {  # only add this curve, if it n_items is known
      g <- plot_density(df = df, x = c("Correlation Coefficient" = "value"))
      g <- g + stat_function(fun = pearson_p, mapping = aes(linetype = "Random Data (Pearson's)"), args = list(n = n_obs))
      g <- g + scale_linetype_manual(values = c("Observed Data" = "solid",
                                                "Random Data (Pearson's)" = "dashed"),
                                     name = "Density Estimate in")
    }
  } else {
    g <- plot_heatmap(color_matrix = cors, color_title = correlation_title)
  }

  # make interactive ====
  if (use_js) {
    g <- plotly::ggplotly(p = g)
  }
  return(g)
}


# HELPERS ====

# probability distribution of pearsons, see http://stats.stackexchange.com/questions/191937/what-is-the-distribution-of-sample-correlation-coefficients-between-two-uncorrel
# notice that this is strictly meaningful only for Pearsons's!
pearson_p <- function(r, n) {
  pofr <- ((1 - r^2)^((n - 4)/2))/beta(a = 1/2, b = (n - 2)/2)
  return(pofr)
}
