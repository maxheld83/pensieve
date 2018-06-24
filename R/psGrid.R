# helper ====
#' @title Store sorting grid as logical matrix
#'
#' @description
#' Stores sorting grid as logical matrix with sorting columns as columns, sorting rows as rows and `TRUE` (allowed) or `FALSE` (not allowed) in cells.
#'
#' @details
#' *Every* sort must have a grid.
#' Even a free distribution must have a grid, giving the maximum indices of rows and columns, but with all cells `TRUE`.
#'
#' @param grid
#' A logical matrix giving the available cells for Q sorting.
#' Accepts arbitrary dimnames from grid.
#' If any dimnames are missing, sensible defaults will be set.
#'
#' @param pattern
#' A character string, giving the pattern of tesselation to use.
#' Must be `"chessboard"` (default), the only currently supported pattern.
#'
#' @param offset
#' A character string, giving the rows to be offset.
#' Must be `"even"`, `"odd"` or `NULL` (default).
#' Applies only to `"honeycomb"` and `"brickwall"` patterns, otherwise ignored.
#'
#' @example tests/testthat/helper_psGrid.R
#'
#' @family S3 classes from `pensieve`
#'
#' @return A logical matrix of class [psGrid][psGrid].
#'
#' @export
# TODO it's not clear that we even need "honeycomb" or "brickwall"; perhaps *both* are saved as axial coordinates, and only the shape is slightly different https://www.redblobgames.com/grids/hexagons/#coordinates, proper term would be hexagonal binning
# TODO is it really brickwall tesselation? what is the real term for this?
# TODO in case of hex coord system, you might also need pointy top or flat top as a variable
psGrid <- function(grid,
                   pattern = "chessboard",
                   offset = NULL) {
  if (is.null(rownames(grid))) {
    rownames(grid) <- LETTERS[1:nrow(grid)]
  }
  if (is.null(colnames(grid))) {
    if (is_even(ncol(grid))) {
      # this is awkward, seems legit only when unipolar sort, so we take 1:n
      colnames(grid) <- as.character(1:ncol(grid))
    } else {
      extreme <- ncol(grid) %/% 2
      colnames(grid) <- as.character(-extreme:extreme)
    }
  }
  grid <- new_psGrid(grid = grid, pattern = pattern, offset = offset)
  assert_S3(grid)
  return(grid)
}

# constructor
new_psGrid <- function(grid, pattern, offset) {
  assert_matrix(x = grid, mode = "logical")
  assert_string(x = pattern)
  assert_string(x = offset, null.ok = TRUE)

  structure(
    .Data = grid,
    pattern = pattern,
    offset = offset,
    class = c("psGrid", "matrix"))
}

#' @describeIn psGrid Validation
#' @inheritParams validate_S3
#' @export
validate_S3.psGrid <- function(x, ps_coll = NULL, ...) {
  assert_matrix(
    x = x,
    mode = "logical",
    any.missing = FALSE,
    all.missing = FALSE,
    null.ok = FALSE,
    add = ps_coll,
    .var.name = "grid"
  )
  assert_names2(x = colnames(x), type = "unique", add = ps_coll, .var.name = "grid")
  assert_names2(x = rownames(x), type = "unique", add = ps_coll, .var.name = "grid")

  assert_choice(
    x = attr(x = x, which = "pattern"),
    choices = c("honeycomb", "chessboard", "brickwall"),
    null.ok = FALSE,
    .var.name = "grid",
    add = ps_coll
  )

  assert_choice(
    x = attr(x = x, which = "offset"),
    choices = c("even", "odd"),
    null.ok = TRUE,
    .var.name = "grid",
    add = ps_coll
  )

  NextMethod(ps_coll = ps_coll)
}

# coercion ====
#' @rdname psGrid
#' @param obj
#' An object which can be coerced to a logical matrix of class [psGrid][psGrid], currently one of:
#' - a (named) integer(ish) vector, giving the column height of `TRUE`s from the bottom (names are retained as column names),
#' - a logical matrix, as per [psGrid].
#' @export
as_psGrid <- function(obj, ...) {
  UseMethod("as_psGrid")
}
as_psGrid.default <- function(obj, ...) {
  stop(
    "Sorry, don't know how to coerce object of class ",
    class(obj),
    "."
  )
}
as_psGrid.psGrid <- function(obj, ...) {
  obj
}
#' @describeIn psGrid Coercion
#' @export
as_psGrid.integer <- function(obj, ...) {
  # input validation
  assert_integer(x = obj, lower = 0, any.missing = FALSE, null.ok = FALSE)
  assert_names2(x = names(obj), type = "unique")

  overall_height <- max(obj)

  # purrr isn't good for this job because it only returns tibbles; overkill here
  m <- sapply(X = obj, FUN = function(this_height) {
    this_column <- c(rep(FALSE, overall_height - this_height), rep(TRUE, this_height))
    return(this_column)
  })

  m <- matrix(data = m, nrow = max(obj), ncol = length(obj), dimnames = list(y = NULL, x = names(obj)))

  psGrid(grid =  m, ...)
}
#' @rdname psGrid
#' @export
as_psGrid.numeric <- function(obj, ...) {
  if (test_integerish(x = obj)) {
    as_psGrid(obj = as.integer(obj), ...)
  } else {
    NextMethod()
  }
}
#' @rdname psGrid
#' @export
as_psGrid.matrix <- function(obj, ...) {
  psGrid(grid = obj, ...)
}

# print ====
#' @describeIn psGrid Printing inside knitr chunks
#' @param header A logical flag, defaults to `TRUE`, in which case column names  from `grid` are included as headers.
#' @param footer A logical flag, defaults to `TRUE`, in which case column names  from `grid` are included as footers.
#' @param aspect_ratio_cards
#' A numeric scalar, giving width divided by height of *individual cards* (such as 16/9 for screen dimensions).
#' Aspect ratio of *cards* is required to appropriately set the resulting dimensions of the *grid*.
#' Defaults to standard business cards.
#' @param inline
#' A logical flag indicating whether knitr is called from *inline* (`r 1+1`) or from a chunk.
#' Defaults to `FALSE`.
#' @inheritParams knitr::knit_print
#' @export
knit_print.psGrid <- function(x,
                              header = TRUE,
                              footer = TRUE,
                              aspect_ratio_cards = 85/54,
                              inline = FALSE,
                              ...) {
  if (inline) {
    #TODO this currently does not really work https://github.com/maxheld83/pensieve/issues/385
    # makes no sense / is complicated to print html5_grid inline, so we pass on to default knit_print method for matrix
    NextMethod()
  } else {
    if (knitr::is_html_output()) {
      knitr::knit_print(
        x = html5_grid(
          grid = x,
          header = header,
          footer = footer,
          aspect_ratio_cards = aspect_ratio_cards),
        ...
      )
    } else {
      # no special idea about this format, so pass it on
      NextMethod()
    }
  }
}
