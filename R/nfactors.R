#' @title Draw one random Q-sort dataset.
#'
#' @export
#'
#' @description Draw one random Q-sort dataset, parametrized as original data.
#'
#' @param grid
#' A positive integer vector of a length covering the range of values, specifying maximum allowed frequencies for each value.
#' (in Q-parlance, the maximum column heights for the Q-sorts).
#'
#' @param n A positive integer vector of length 1, as the number of people-variables.
#' Defaults to `NULL`, in which case parameter is inferred from `data`.
#'
#' @param p A positive integer vector of length 1, as the number of item-cases.
#' Defaults to `NULL`, in which case parameter is inferred from `data`.
#'
#' @keywords internal
#'
#' @details
#' This function draws a *random* dataset, parametrized as some real or hypothetical *observed* dataset from a Q study.
#'
#' Q data has some peculiarities that must be implemented to draw comparable random datasets, and that are not addressed in existing packages implementing parallel analysis (such as [paran::paran()]):
#'
#' - **Integers.**
#'   The simulated data include only integers.
#' - **Draws without replacement.**
#'   Because two items cannot occupy the same position in a Q-sort, random data must be drawn *without replacement* from the grid of available item positions (slots).
#' - **Forced or free distribution.**
#'   Depending on whether the observed data resulted from a forced or free distribution, random data must be drawn from the appropriate available item positions (slots).
#'
#' Helper function for [run_parallel()].
#'
#' @note
#' Notice that in the case of a free distribution, there are *no* assumptions on the distributional shape (such as uni-modal, bimodal etc.).
#' Random data for a free distribution are therefore drawn from an (on average over all people-variables) *rectangular* distribution *over all available slots*, as a *neutral* distributional shape.
#' A more sophisticated parallel analysis may first infer some observed distribution parameters and then draw random data accordingly as per [issue 146 on the development repo](https://github.com/maxheld83/pensieve/issues/146).
#'
#' @return
#' A random integer matrix with item-cases as rows and people-variables as columns.
#'
#' @family parallel-analysis
#'
#' @author Maximilian Held
#'
draw_rand_sort <- function(grid = NULL,  # named integer vector of grid
                           n = NULL,  # number of people-variables
                           p = NULL) { # number of item-cases

  # input validation ====
  expect_vector(x = grid,
                strict = TRUE,
                any.missing = FALSE,
                all.missing = FALSE,
                names = "unique",
                null.ok = FALSE)
  expect_integerish(x = grid,
                    min.len = 1,
                    lower = 0)

  expect_count(x = n,
               na.ok = FALSE,
               positive = TRUE,
               null.ok = FALSE)

  expect_lte(object = p,
             expected = sum(grid))
  expect_count(x = p,
               na.ok = FALSE,
               positive = TRUE,
               null.ok = FALSE)

  # body ====
  slots <- rep(x = as.integer(names(grid)), times = grid)
  draw <- sapply(X = 1:n,
                 FUN = function(x) {
                   sample(x = slots, size = p, replace = FALSE)
                 })
  dimnames(x = draw) <- list(items = NULL, people = NULL)
  return(draw)
}


#' @title Extract many random matrix eigenvalues.
#'
#' @export
#'
#' @param dataset
#' An integer matrix with item-cases as rows and people-variables as columns.
#' Defaults to `NULL`, in which case parameters must be passed to `draw_rand_sort()` using `...`.
#'
#' @param centile
#' A positive numerical vector of length 1, as the percentile of random eigenvalues to return.
#' Defaults to the conventional significance threshold of `.95`.
#'
#' @param runs
#' A positive integer vector of length 1, as the number of random data to draw, defaults to `5000`.
#' Lower number will reduce computational cost, but results may be less reliable.
#'
#' @param cutoff
#' A positive integer vector of length 1, as the maximum number of Principal Components to extract from random data.
#' Defaults to `NULL`, in which case the minimum of `n` and `p` are used, because there can be no more Principal Components than there are item-cases `or` people-variables in a dataset.
#' Manually specifying a lower number may reduce the computational cost.
#'
#' @inheritParams draw_rand_sort
#'
#' @description Extract eigenvalues from randomly generated data, parametrized as original data.
#'
#' @keywords internal
#'
#' @aliases run_parallel
#'
#' @details
#' Implements Horn's (1965) parallel analysis as a guide to inform factor retention, with appropriate parametrization for Q-sort data (via [draw_rand_sort()]).
#'
#' Random data can include spurious correlations and spurious factors by mere chance.
#' This problem might affect Q studies, too: people might produce similar Q sorts not because they share viewpoints, but just out of random chance.
#' Parallel analysis tests this assertion by extracting (spurious) principal components from many sets of random data, similarly parametrized as the provided `data`.
#'
#' The analysis returns the random eigenvalues at the specified `centile` over all of the specified random `runs`.
#' This result can be interpreted as a necessary Eigenvalue threshold for some *observed* principal component to be considered non-spurious, with the specified centile as the confidence (or significance level).
#'
#' In summary, parallel analysis suggests the number of factors in the data that are unlikely to be products of random chance at some significance level `centile`.
#'
#' For more details, consider the related [paran package](https://cran.r-project.org/web/packages/paran/index.html).
#'
#' @references
#' - Glorfeld, L. W. 1995: *An Improvement on Horn-s Parallel Analysis Methodology for Selecting the Correct Number of Factors to Retain*, Educational and Psychological Measurement. 55(3): 377-393.
#' - Horn, J. L. (1965): *A rationale and a test for the number of factors in factor analysis*, Psychometrika. 30: 179-185.
#'
#' @note
#' This function is currently based on principal components analysis (PCA) as a ``factor'' extraction technique.
#' Thompson (2004: 30ff) and others seem to suggest that such PCA-based criteria can be used as rough indications for how many factors to extract with other exploratory techniques.
#' However, some of the results presented here are meaningful *only* in a PCA-context, and dependent functions are sometimes called with PCA-related options.
#'
#' @author Maximilian Held
#'
#' @family parallel-analysis
#'
#' @return A numerical vector of length `cutoff`, with Eigenvalues for consecutive Principal Components.
#'
#' @examples
#' dataset <- civicon_2014$qData$sorts[,,"before"]
#' run_parallel(dataset = dataset,  # parameters are inferred
#'              runs = 10, # way too few, just to make this fast
#'              centile = .95)  # default
#' # results are all the same, because study used forced Q distribution

run_parallel <- function(dataset = NULL,
                         centile = .95,
                         runs = 5000,
                         cutoff = NULL,
                         grid = NULL,
                         n = NULL,
                         p = NULL) {  # passed on to draw_rand_sort

  # data validation and preparation ====
  # we need to make sure that values are overridden from dataset
  if (!is.null(dataset)) {
    if (!is.null(n)) {
      warning("'n' is inferred from 'dataset', entered argument is ignored.")
    }
    n <- ncol(dataset)
    if (!is.null(p)) {
      warning("'p' is inferred from 'dataset', entered argument is ignored.")
    }
    p <- nrow(dataset)
    if (is.null(grid)) {
      grid <- diag_distro_max(dataset)
      warning("Argument 'grid' is inferred from maximum value counts of 'dataset'.
              Consider supplying 'grid' as an argument")
    }
  }

  # is dataset inside grid?
  if (!is.null(grid) & !is.null(dataset)) {
    expect_true(object = diag_inside_grid(sorts = dataset, grid = grid),
                info = "Maximum counts of dataset are not inside grid.")
    # this also takes care of grid and dataset validation
  }

  expect_scalar(x = centile)
  expect_numeric(x = centile,
                 lower = 0,
                 upper = 1,
                 finite = TRUE,
                 any.missing = FALSE,
                 all.missing = FALSE,
                 null.ok = FALSE)

  expect_count(x = runs,
               na.ok = FALSE,
               positive = TRUE,
               null.ok = FALSE)

  if (is.null(cutoff)) {
    cutoff <- min(p, n)
  } else {
    expect_count(x = cutoff,
                 na.ok = FALSE,
                 positive = TRUE,
                 null.ok = FALSE)
    expect_integer(x = cutoff,
                   lower = 1,
                   upper = min(p, n),
                   info = c("Cutoff must not be larger than 'p' and 'n'.
                             there cannot be more Principal Components than people-variables or item-cases."))
  }

  # initialize empty matrix
  runmat <- matrix(data = NA,
                   nrow = runs,
                   ncol = cutoff,
                   dimnames = list(run = NULL, eigenvalue = NULL))

  # progress indicator for loops
  if (interactive()) {
    pbtarget <- ""  # if this is run interactively, send pb to stdout (default)
  } else {
    pbtarget <- stderr()
  }
  pb <- utils::txtProgressBar(min = 0,
                       max = runs,
                       initial = 0,
                       style = 3,
                       file = pbtarget)  # hack job to send this to console while knitting as per http://stackoverflow.com/questions/20009152/knitr-redirect-chunk-code-output-to-terminal

  for (i in 1:runs) {  # for loop allows out of the box progress bar, not much slower
    runmat[i, ] <- stats::prcomp(x = draw_rand_sort(grid = grid, n = n, p = p),
                                 center = TRUE,
                                 scale. = TRUE)$sdev[1:cutoff] ^ 2
    utils::setTxtProgressBar(pb,i)
  }

  res <- apply(X = runmat,
               MARGIN = 2,
               FUN = function(x) {stats::quantile(x = x, probs = centile)})
  return(res)
}
