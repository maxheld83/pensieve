# helper ====
#' @title Store multiple sorts as a numeric matrix
#'
#' @description
#' Canonical storage for closed sorts.
#'
#' @param csorts `[array()]`
#' An numeric array with people as rows, item handles as columns, arbitrary dimensions thereafter (such as for multiple sorting dimensions), and item positions in cells.
#'
#' @example tests/testthat/helper_psClosedSorts.R
#'
#' @family S3 classes from `pensieve`
#'
#' @return A numeric matrix of class [psClosedSorts][psClosedSorts].
#'
#' @export
psClosedSorts <- function(csorts) {
  csorts <- new_psClosedSorts(csorts = csorts)
  assert_S3(csorts)
  return(csorts)
}

# constructor
new_psClosedSorts <- function(csorts, ...) {
  # assert base type
  assert_array(
    x = csorts,
    mode = "numeric",
    any.missing = TRUE,
    min.d = 2,
    null.ok = FALSE
  )

  structure(
    .Data = csorts,
    class = c("psClosedSorts", class(csorts))
  )
}

#' @describeIn psClosedSorts Validation against items and grid (recommended)
#' @inheritParams validate_S3
#' @inheritParams psSort
#' @export
validate_S3.psClosedSorts <- function(x, items = NULL, grid = NULL, ...) {
  walk(.x = dimnames(x), .f = function(x) {
    assert_names2(x = x, type = "strict")
  })

  NextMethod(ps_coll = ps_coll)
}

# PLOTTING ====
#' @describeIn psClosedSorts plotting
#'
#' @export
#'
#' @template plot
#'
#' @inheritParams psClosedSorts
#'
#' @param column
#' Positive integer scalar, giving the column of the psClosedSorts object to plot.
#' Defaults to `1`, in which case the first column is plotted.

plot.psClosedSorts <- function(x, column = 1, use_js = NULL, ...) {
  # Init (for testing) ====
  # x <- sorts
  # column <- 1
  # use_js <- NULL

  # Input validation ====
  sorts <- psClosedSorts(csorts = x)
  use_js <- assert_n_infer_use_js(use_js = use_js)

  # Data Prep ====
  sort <- sorts[,column]

  # Plotting ====
  g <- plot.QSort(x = sort, type = "grid", use_js = use_js)
  return(g)
}


plot.QSort <- function(x, type = "grid", use_js = NULL) {
  # Initialisation (for testing ) =====
  # x <- sorts[,1]

  # Input validation ====
  # TODO validate x
  assert_choice(x = type, choices = c("grid", "brickwall", "hex"))
  use_js <- assert_n_infer_use_js(use_js = use_js)

  # Data prep ====
  xy <- spread_over_y(x = x)
  xy <- as.data.frame(xy)
  xy$item <- rownames(xy)

  # plotting ====
  y <- x <- item <- NULL  # hack to appease R cmd check
  g <- NULL
  g <- ggplot(data = xy, mapping = aes(x = y, y = x, label = item, fill = "1", color = "2"))
  # g <- g + geom_raster(fill = "red")
  g <- g + geom_tile()
  g <- g + geom_text()
  g <- g + scale_fill_manual(values = c("white"))
  g <- g + scale_color_manual(values = c("black"))

  if (use_js) {
    g <- plotly::ggplotly(g)
  }
  return(g)
}

spread_over_y <- function(x) {
  x_range <- max(x) - min(x)
  y_rep <- round(length(x)/x_range + 1)
  y_vals <- rep(c(min(x):max(x)), y_rep)[1:length(x)]
  bothdims <- cbind(x, y = y_vals)
  return(bothdims)
}
