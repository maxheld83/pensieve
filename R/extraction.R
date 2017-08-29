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
QLoas <- produce_class_constructor(classname = "QLoas", fun = function(loas) {
  return(loas)
})

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


# PLOTTING ====
#' @describeIn extract plotting
#'
#' @export
#'
#' @template plot
#'
#' @param by
#' A character string indicating by which dimension to plot.
#' Must be `"people"` or `"both"`.
#' Display by `"factor"` is not currently available.
#'
#' @param r2
#' Logical flag, indicating whether the *squared* loadings should be plotted.
#' If `FALSE`, loadings are plotted, interpretable as correlations of people-variables with factors.
#' If `TRUE`, squared loadings are plotted, interpretable as the share of explained variance.
#' Defaults to `FALSE`.
#'
#' @inheritParams plot.QCors
#'
#' @examples
#' # makes a barchart
#' plot(x = loas, use_js = NULL, by = "people")
#' plot(x = loas, use_js = NULL, by = "both")
#'
plot.QLoas <- function(x, summarize = NULL, by = "people", r2 = FALSE, use_js = NULL, ...) {
  # Input validation ====
  loas <- QLoas(loas = x, validate = TRUE)  # make and validate QCors first

  use_js <- assert_n_infer_use_js(use_js = use_js)

  summarize <- assert_n_infer_summarize(summarize = summarize, x = x)

  checkmate::assert_flag(x = r2,
                         na.ok = FALSE,
                         null.ok = FALSE)

  checkmate::assert_choice(x = by,
                           choices = c("people", "both"))


  # Data preparation ====
  if (r2) {
    loas <- loas^2  # hacky, because then they are technically no longer loadings ...
  }
  loas_molten <- reshape2::melt(data = loas, varnames = c("Sort", "Factor"), value.name = "Loading")
  loas_molten$Factor <- factor(loas_molten$Factor)  # order according to sequence in extraction

  # Plotting ====

  if (by == "both") {
    if (summarize) {
      g <- plot_density(df = loas_molten, x = "Loading", color = "Factor")
    } else {
      g <- plot_heatmap(color_matrix = loas, color_title = "Loadings")
    }
  } else if (by == "people") {
    if (summarize) {
      if (r2) {
        g <- plot_density(df = loas_molten, x = c("Squared Loadings" = "Loading"), color = "Factor")
      } else {
        g <- plot_density(df = loas_molten, x = c("Loadings" = "Loading"), color = "Factor")
      }
    } else {
      if (r2) {
        g <- plot_barchart_area(df = loas_molten, x = "Loading", y = "Sort", fill = "Factor", flip = TRUE)
      } else {
        g <- plot_barchart_length(df = loas_molten, x = "Loading", y = "Sort", fill = "Factor", flip = TRUE)
      }
    }

  }

  # make interactive ====
  if (use_js) {
    g <- plotly::ggplotly(p = g)
  }
  return(g)
}
