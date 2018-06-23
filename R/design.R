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
#' @examples
#' # make simple matrix by hand
#' m <- matrix(data = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE), nrow = 2, ncol = 3)
#' grid <- psGrid(grid = m, pattern = "chessboard")
#'
#' @family S3 classes from `pensieve`
#'
#' @return A logical matrix of class `psGrid`.
#'
#' @export
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

as_psGrid <- function(x, ...) {
  UseMethod("as_psGrid")
}
as_psGrid.default <- function(x, ...) {
  stop(
    "Sorry, don't know how to coerce object of class ",
    class(x),
    "."
  )
}
as_psGrid.psGrid <- function(x, ...) {
  x
}
as_psGrid.integer <- function(x, ...) {
  # input validation
  assert_integer(x = x, lower = 0, any.missing = FALSE, null.ok = FALSE)
  assert_names2(x = names(x), type = "unique")

  overall_height <- max(x)

  # purrr isn't good for this job because it only returns tibbles; overkill here
  m <- sapply(X = x, FUN = function(this_height) {
    this_column <- c(rep(FALSE, overall_height - this_height), rep(TRUE, this_height))
    return(this_column)
  })

  m <- matrix(data = m, nrow = max(x), ncol = length(x), dimnames = list(y = NULL, x = names(x)))

  psGrid(grid =  m, ...)
}
as_psGrid.numeric <- function(x, ...) {
  if (test_integerish(x = x)) {
    as_psGrid(x = as.integer(x), ...)
  } else {
    NextMethod()
  }
}
