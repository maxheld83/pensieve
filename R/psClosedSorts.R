# construction ====
#' @title Store multiple sorts as a numeric matrix
#'
#' @description
#' Canonical storage for closed sorts.
#'
#' @param csorts `[matrix()]`
#' An numeric matrix with people as rows, item handles as columns and item positions in cells.
#'
#' @example tests/testthat/helper_01_psGrid.R
#' @example tests/testthat/helper_03_psSort.R
#' @example tests/testthat/helper_04_psClosedSorts.R
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
  assert_matrix(
    x = csorts,
    mode = "numeric",
    any.missing = TRUE,
    null.ok = FALSE
  )

  structure(
    .Data = csorts,
    class = c("psClosedSorts", "matrix")
  )
}

# validation ====

#' @describeIn psClosedSorts Validation against items and grid (recommended)
#' @inheritParams validate_S3
#' @inheritParams psSort
#' @export
validate_S3.psClosedSorts <- function(x, items = NULL, grid = NULL, ...) {
  walk(.x = dimnames(x), .f = function(x) {
    assert_names2(x = x, type = "strict", add = ps_coll, .var.name = "x")
  })

  if (!is.null(items)) {
    items <- as_psItemContent(items)
    assert_named(x = items, add = ps_coll, .var.name = "items")
    assert_set_equal(
      x = colnames(x),
      y = names(items),
      add = ps_coll,
      .var.name = "items"
    )
  }

  if (!is.null(grid)) {
    grid <- as_psGrid(grid)

    # the validations already all exist in the below functions, so we reuse them here
    iwalk(.x = tibble::as_tibble(t(x)), .f = function(one_row, name) {
      # add item names to row again, these are lost in above transformation
      names(one_row) <- colnames(x)
      class(one_row) <- NULL  # this triggers false coercion method otherwise
      # run existing coercion to capture errors
      suppressWarnings(
        assert_fun_args(
          x = as_psSort,
          y = one_row,
          grid = grid,
          add = ps_coll,
          .var.name = name
        )
      )
    })
  }

  NextMethod(ps_coll = ps_coll)
}

# coercion ====
#' @rdname psClosedSorts
#' @param obj
#' An object which can be coerced to an integer array of class [psClosedSorts][psClosedSorts].
#' @export
as_psClosedSorts <- function(obj, ...) {
  UseMethod("as_psClosedSorts")
}
as_psClosedSorts.default <- function(obj, ...) {
  stop_coercion(obj = obj, target_class = "psClosedSorts")
}
as_psClosedSorts.psClosedSorts <- function(obj, ...) {
  assert_S3(x = obj)
  obj
}
#' @describeIn psClosedSorts Coercion from [psSort][psSort] (creates one row)
#' @export
as_psClosedSorts.psSort <- function(obj, items = NULL, ...) {
  assert_S3(obj)

  # TODO this is a bit of hack job, maybe fix this
  # when there *are* dimnames, these would be taken as x coords, which they are not
  # we only want indices
  dimnames(obj) <- NULL

  res <- reshape2::melt(obj, na.rm = TRUE)

  if (!is.null(obj %@% "offset")) {
    # add offsets
    if (obj %@% "offset" == "even") {
      res[is_even(res[[1]]),2] <- res[is_even(res[[1]]),2] + 0.5
    }
    if (obj %@% "offset" == "odd") {
      res[!is_even(res[[1]]),2] <- res[!is_even(res[[1]]),2] + 0.5
    }
    res[,2] <- res[,2] * 2  # make sure we have integers again
  }

  m <- matrix(
    data = res[[2]],  # x coordinates
    nrow = 1
  )
  colnames(m) <- as.character(res[[3]])

  if (!is.null(items)) {
    items <- as_psItemContent(items)
    assert_subset(x = colnames(m), choices = names(items))

    m_w_all_items <- matrix(
      data = NA,
      nrow = 1,
      ncol = length(items),
      dimnames = list(NULL, items = names(items))
    )
    m_w_all_items[, colnames(m)] <- m

    m <- m_w_all_items
  }

  psClosedSorts(m)
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
