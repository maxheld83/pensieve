# QPreSorts ====

#' @title Check and make QPreSorts
#'
#' @export
#'
#' @description Checks and makes QPreSorts
#'
#' @param presorts
#' An integer matrix, with named rows as item handles, named columns as participant names and cells as presorts.
#' `-1L` for `negative`, `0L` for `neutral` and `1L` for `positive`.
#'
#' @template construct
#'
#' @note
#' `presorts` are stored as `integer()` because R does not allow factor matrices.
#' Pre-sorting piles are, of course, `categorical` information and should be treated as such.
#'
#' @family import helpers
QPreSorts <- produce_class_constructor(classname = "QPreSorts", fun = function(presorts) {
  return(presorts)
})

#' @export
#' @rdname check
check.QPreSorts <- function(x) {
  res <- NULL  # appease R

  res$matrix <- check_matrix(x = x,
                             mode = "integer",
                             any.missing = TRUE,
                             all.missing = FALSE,
                             row.names = "strict",
                             col.names = "strict")
  res$range <- check_integer(x = x,
                             any.missing = TRUE,
                             lower = -2,
                             upper = 2)

  return(report_checks(res = res, info = "QPreSorts"))
}


# QSorts ====
#' @title Check and make QSorts
#'
#' @export
#'
#' @description Checks and makes QSorts
#'
#' @param sorts
#' An integer array with item handles as first dimension, people as second dimension, arbitrary dimensions thereafter, and item positions in cells.
#' Dimensions must be named.
#'
#' @template construct
#'
#' @family import helpers
QSorts <- produce_class_constructor(classname = "QSorts", fun = function(sorts) {
  return(sorts)
})

#' @describeIn QSorts validation
#'
#' @export
#'
#' @template check
#'
#' @examples
#' sorts <- civicon_2014$qData$sorts[,,"before"]
#' sorts <- QSorts(sorts = sorts, validate = FALSE)
#' check(x = sorts)
check.QSorts <- function(x) {
  res <- NULL

  res$array <- check_array(x = x,
                           mode = "integer",
                           any.missing = TRUE,
                           min.d = 2,
                           null.ok = FALSE)
  res <- c(res, check_named_array(x = x))  # via external helper

  return(report_checks(res = res, info = "QSorts"))
}

# PLOTTING ====
#' @describeIn QSorts plotting
#'
#' @export
#'
#' @template plot
#'
#' @inheritParams QSorts
#'
#' @param column
#' Positive integer scalar, giving the column of the QSorts object to plot.
#' Defaults to `1`, in which case the first column is plotted.
#'
#' @examples
#' plot(x = sorts)

plot.QSorts <- function(x, column = 1, use_js = NULL, ...) {
  # Init (for testing) ====
  # x <- sorts
  # column <- 1
  # use_js <- NULL

  # Input validation ====
  sorts <- QSorts(sorts = x, validate = TRUE)
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
