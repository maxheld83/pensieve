# TODO actually this individual form would work for psOpenSort AND psClosedSort as well!
# these should be subclasses to the more general psSort, and only differentiate in the methods as far as necessary
# the documentation below is as far as possible already generic, but this needs refactoring.

# helper ===
#' @title Store an *individual*  sort (Q-sort) as a character matrix.
#' @description
#' Stores *one* sort, by one participant as a character \eqn{i * k} matrix, with sorting columns as columns, sorting rows as rows and *short item handles* (see `psItems`) in cells.
#'
#' @details
#' Sorts can be stored in the form in which they were originally created on a table or in a computer user interface.
#' The `y`-axis, though meaningless (ties) in most studies, is also stored, but this full matrix form makes it easy to reason about the data, and to validate it.
#'
#' Storing sorts of *several* participants as *lists* of such `psSort`s (as is done in `psData`) also makes it possible to record sorts where not all participants used exactly the same sorting template (different `psGrid`s or tesselation patterns).
#'
# TODO link to these methods, and find precise VERBS for these transformations.
#' Methods are included to rbind `psItems` to `psData`, as well as to transform them into a tidy matrix.
#'
#' @param sort
#' A character matrix giving the item positions of the sort.
#' Items must be identified by their item short handles and must be unique.
#' If `grid` is specified, only `TRUE` cells can have items.
#'
#' @inheritParams psGrid
#'
#' @family S3 classes from `pensieve`.
#'
#' @return A character matrix of class [psSort][psSort].
#'
#' @example tests/testthat/helper_psSort.R
#'
#' @export
psSort <- function(sort, grid = NULL, items = NULL, pattern = "chessboard", offset = NULL) {

  # create default variables
  sort <- new_psSort(sort = sort, pattern = pattern, offset = offset)
  assert_S3(sort)
  return(grid)
}

new_psSort <- function(sort, pattern, offset) {
  structure(
    .Data = sort,
    pattern = pattern,
    offset = offset,
    class = c("psSort", "matrix")
  )
}

#' @describeIn psSort Validation
#' @inheritParams validate_S3
#' @inheritParams psGrid
#' @inheritParams psItemContent
#' @export
validate_S3.psSort <- function(x, grid = NULL, items = NULL, ps_coll = NULL, ...) {
  # assert base type
  assert_matrix(
    x = x,
    mode = "character",
    any.missing = TRUE,
    all.missing = TRUE, # useful for initialising
    null.ok = FALSE,
    add = ps_coll
  )

  assert_names2(x = colnames(x), type = "unique", add = ps_coll, .var.name = "sort")
  assert_names2(x = rownames(x), type = "unique", add = ps_coll, .var.name = "sort")

  #TODO there is some repetition in here with psGrid; perhaps this really ought to be one class with subclasses?!?
  assert_choice(
    x = attr(x = x, which = "pattern"),
    choices = c("honeycomb", "chessboard", "brickwall"),
    null.ok = FALSE,
    .var.name = "sort",
    add = ps_coll
  )
  assert_choice(
    x = attr(x = x, which = "offset"),
    choices = c("even", "odd"),
    null.ok = TRUE,
    .var.name = "sort",
    add = ps_coll
  )

  # infer and coerce other variable
  if (is.null(grid)) {
    grid <- x
    grid[,] <- TRUE
    #TODO  below line should be part of normal coercion method
    storage.mode(grid) <- "logical"
    grid <- as_psGrid(obj = grid)
  }

  # check VS grid
  # check if sort rank corresponds to grid rank
  assert_matrix(
    x = x,
    nrows = nrow(grid),
    ncols = ncol(grid),
    add = ps_coll
  )

  # check VS items
  #TODO coerce psItemContent! this is not validated right now!
  if (!is.null(items)) {
    # check that there are enough cells for all items
    assert_vector(x = items, max.len = length(x), add = ps_coll)
  }

  NextMethod(ps_coll = ps_coll)
}


#' @title Place item into row and column of a closed sort.
#' @param csort A matrix of class (or coercable to) `psClosedSort`, in *wide* form, with rows as ties, columns as ranks and item handles in cells.
#' @param row An integer scalar giving the row index.
#' @param column An integer scalar giving the column index.
#' @param item A character string giving the item handle.
#' Defaults to `NA`, in which case `row` and `column` cell is set to `NA`.
#' Useful for *removing* items.
#' @param grid A logical matrix of class (or coercable to) `psGrid`.
#' Defaults to `NULL`, in which case all cells in `csort` are allowed as `TRUE`.
#' Useful for *free* distributions.
#' @param items A character vector giving *possible* item handles.
#' @return A matrix of class `psClosedSort`.
#' @noRd
append_psClosedSort <- function(csort, row, column, item = NA, grid = NULL, items) {
  # input validation
  assert_matrix(
    x = csort,
    mode = "character",
    any.missing = TRUE,
    all.missing = TRUE,
    null.ok = FALSE)
  # TODO in future, function should allow NULL rows, because rows are actually meaningless, and some upstream uses may not have row info, such as when data is only in long form already
  assert_scalar(x = row, na.ok = FALSE, null.ok = FALSE)
  assert_scalar(x = column, na.ok = FALSE, null.ok = FALSE)
  assert_string(x = item, na.ok = TRUE, null.ok = FALSE)
  assert_matrix(x = grid, mode = "logical", any.missing = FALSE, null.ok = TRUE)
  assert_character(x = items, any.missing = FALSE, unique = TRUE, null.ok = FALSE)

  # prepare values
  item <- as.character(item)
  if (is.null(grid)) {
    grid <- matrix(data = TRUE, nrow = nrow(csort), ncol = ncol(csort))
  }

  # TODO some of this might be better tested in wrappers; would be called too often in here
  # consistency checks
  assert_matrix(
    x = csort,
    nrows = nrow(grid),
    ncols = ncol(grid)
  )
  assert_integer(x = row, lower = 0, upper = nrow(csort))
  assert_integer(x = column, lower = 0, upper = ncol(csort))
  assert_choice(x = item, choices = c(items, NA))
  assert_character(x = items, max.len = sum(grid))

  if (!is.na(item)) {
    # item must not already be placed in csort
    assert_false(x = item %in% csort, na.ok = TRUE)
  }
  assert_true(x = grid[row, column], na.ok = FALSE)

  csort[row, column] <- item
  return(csort)
}
