# CALCULATION ====
#' @title Create Q sorting grid.
#'
#' @description Creates a grid for Q sorting.
#'
#' @export
#'
#' @param x_range
#' An integer vector of length two, giving the range on the x axis.
#' Defaults to `c(-5, 5)`.
#'
#' @param y_range
#' An integer vector of length two, giving the range on the y axis.
#' Defaults to `c(1, 5)`.
#'
#' @inherit psGrid params
#'
#' @family design functions
#' @family survey functions
#'
#' @examples
#' # this makes the grid, already assigns class
#' grid <- make_grid(x_range = c(-5, 5), y_range = c(1,5), pattern = "honeycomb", offset = "odd")

make_grid <- function(x_range = c(-5,5), y_range = c(1,5), pattern = "chessboard", offset = "odd") {
  # Initialisation (for testing only) ====
  if (FALSE) {
    x_range <- c(-5,5)
    y_range <- c(1,5)
    pattern <- "honeycomb"
    offset <- "even"
  }

  # Input validation ====

  lapply(X = list(x_range, y_range), FUN = function(x) {
    assert_vector(x = x,
                  strict = FALSE,
                  any.missing = FALSE,
                  all.missing = FALSE,
                  len = 2,
                  unique = TRUE,
                  null.ok = FALSE)
    assert_integerish(x = x)
    return(NULL)
  })
  assert_choice(x = pattern,
                choices = c("chessboard", "honeycomb", "brickwall"))
  assert_choice(x = offset,
                choices = c("even", "odd"))

  # Data preparation ====
  x_values <- c(min(x_range):max(x_range))
  y_values <- c(min(y_range):max(y_range))
  m <- matrix(data = TRUE, nrow = length(y_values), ncol = length(x_values), dimnames = list(y = as.character(y_values), x = as.character(x_values)))

  m <- psGrid(grid = m, pattern = pattern, offset = offset)

  # Return object ====
  return(m)
}

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
#' @param grid A logical matrix giving the available cells for Q sorting.
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
#' m <- matrix(data = c(TRUE, FALSE, FALSE, TRUE), nrow = 2, ncol = 2)
#' grid <- psGrid(grid = m, pattern = "chessboard")
#'
#' @family S3 classes from `pensieve`
#'
#' @return A logical matrix of class `psGrid`.
#'
#' @export
psGrid <- function(grid, pattern = "chessboard", offset = NULL) {
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
