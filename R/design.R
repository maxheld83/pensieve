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
#' @inherit QGrid params
#'
#' @family design functions
#' @family survey functions
#'
#' @examples
#' # this makes the grid, already assigns class
#' grid <- make_grid(x_range = c(-5, 5), y_range = c(1,5), pattern = "honeycomb", offset = "odd")

make_grid <- function(x_range = c(-5,5), y_range = c(1,5), pattern = "honeycomb", offset = "odd") {
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

  m <- QGrid(grid = m, pattern = pattern, offset = offset, validate = TRUE)

  # Return object ====
  return(m)
}

# S3 CLASSES ====
#' @rdname make_grid
#'
#' @export
#'
#' @template construct
#'
#' @param grid A logical matrix giving the available cells for Q sorting.
#'
#' @param pattern
#' A character string, giving the pattern of tesselation to use.
#' Must be `"honeycomb"` (default), the only currently supported pattern.
#' Defaults to `NULL`, in which case `pattern` is expected as an attribute of `grid`.
#'
#' @param offset
#' A character string, giving the rows to be offset.
#' Must be `"even"` or `"odd"` (default).
#' Applies only to `"honeycomb"` and `"brickwall"` patterns.
#'
#' @examples
#' # make simple matrix by hand
#' m <- matrix(data = c(TRUE, FALSE, FALSE, TRUE), nrow = 2, ncol = 2)
#' # assign class, without validation (not recommended)
#' grid <- QGrid(grid = m, pattern = "honeycomb", offset = "even", validate = FALSE)
QGrid <- function(grid, pattern = "honeycomb", offset = "even", validate = TRUE) {
  # Assign attributes
  attr(x = grid, which = "pattern") <- pattern
  if (pattern == "honeycomb" | pattern == "brickwall") {
    attr(x = grid, which = "offset") <- offset
  }

  # assign class
  grid <- classify_clever(x = grid, classname = "QGrid")
  assert_class2(x = grid, validate = validate)

  # return
  return(grid)
}

#' @describeIn make_grid validation
#'
#' @export
#'
#' @template check
#'
#' @examples
#' # validate the class
#' check(grid)
check.QGrid <- function(x) {
  res <- NULL

  res$matrix <- check_matrix(x = x,
                             mode = "logical",
                             any.missing = FALSE,
                             all.missing = FALSE)
  res$pattern <- check_choice(x = attr(x = x, which = "pattern"),
                              choices = c("honeycomb", "chessboard", "brickwall"))
  res$offset <- check_choice(x = attr(x = x, which = "offset"),
                             choices = c("even", "odd"))

  return(report_checks(res = res, info = "QGrid"))
}
