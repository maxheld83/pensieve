# TODO actually this individual form would work for psOpenSort AND psClosedSort as well!
# these should be subclasses to the more general psSort, and only differentiate in the methods as far as necessary
# the documentation below is as far as possible already generic, but this needs refactoring.

# helper ====
#' @title Store an *individual* sort as a character matrix of *item handles*
#' @description
#' Stores *one* sort, by one participant as an \eqn{i * k} character matrix, with sorting columns as columns, sorting rows as rows and *short item handles* (see `psItems`) in cells.
#'
#' @details
#' Sorts can be stored in the form in which they were originally created on a table or in a computer user interface.
#' The `y`-axis, though meaningless (ties) in most studies, is also stored, but this full matrix form makes it easy to reason about the data, and to validate it.
#'
# this argument is almost the same as grid for psGrid; some duplication
#' @param sort `[matrix()]`
#' giving the occupying item of cells as `character(1)` strings of **item handles**.
#' At least one dimension should be named (see examples), or the x-axis (column names) is assumed to be the sorting direction.
#' Unnamed dimensions are assumed to be meaningless, i.e. used for stacking tied items.
#'
#' @param desc_x,desc_y `[character(1)]`
#' giving a description of the sorting dimensions, such as the condition of instruction.
#' Defaults to `NULL`.
#' `desc_x` is for horizontal sorting across the columns.
#' `desc_y` is for (rarely used) vertical sorting across the rows.
#'
#' @inheritParams psGrid
#' @inheritSection psGrid Hexagonal tiling
#' @family S3 classes from `pensieve`.
#' @return A character matrix of class [psSort][psSort].
#' @example tests/testthat/helper_psSort.R
#' @export
psSort <- function(sort, desc_x = NULL, desc_y = NULL, polygon = "rectangle", offset = NULL) {
  sort <- new_psSort(
    sort = sort,
    desc_x = desc_x,
    desc_y = desc_y,
    polygon = polygon,
    offset = offset
  )
  assert_S3(sort)
  return(sort)
}

new_psSort <- function(sort, desc_x, desc_y, polygon, offset) {
  # assert base type
  assert_matrix(
    x = sort,
    mode = "character",
    any.missing = TRUE,
    all.missing = TRUE, # useful for initialising
    null.ok = FALSE
  )

  structure(
    .Data = sort,
    desc_x = desc_x,
    desc_y = desc_y,
    polygon = polygon,
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
  # TODO finish/enable this
  # assert_S3(x = as_psGrid(x), collection = ps_coll, var.name = "sort")
  # reusing some code from psGrid, kinda a bad hack

  # coerce and assert other variables
  if (is.null(grid)) {
    grid <- as_psGrid(obj = x)
  }
  assert_S3(grid)
  if (is.null(items)) {
    items <- as_psItemContent(obj = x)
  }
  assert_S3(items)

  # check x VS grid
  # check if sort rank corresponds to grid rank
  assert_matrix(
    x = x,
    nrows = nrow(grid),
    ncols = ncol(grid),
    add = ps_coll
  )

  # check x VS items
  # check that there are enough cells for all items
  assert_vector(x = items, max.len = length(x), add = ps_coll)
  # test that all items in x are also in items

  # check per cell and per row
  clean_sort <- dirty_sort <- x
  clean_sort[,] <- NA

  # TODO use map here
  for (row in 1:nrow(dirty_sort)) {
    for (column in 1:ncol(dirty_sort)) {
      clean_sort <- append_psSort(sort = clean_sort,
                                  row = row,
                                  column = column,
                                  item = dirty_sort[row, column],
                                  grid = grid,
                                  items = items)
    }
  }
  NextMethod(ps_coll = ps_coll)
}


#' @title Place item into row and column of a closed sort.
#' @inheritParams psSort
#' @param row An integer scalar giving the row index.
#' @param column An integer scalar giving the column index.
#' @param item A character string giving the item handle.
#' Defaults to `NA`, in which case `row` and `column` cell is set to `NA`.
#' Useful for *removing* items.
#' @return A matrix of class `psSort`.
#' @noRd
append_psSort <- function(sort, row, column, item = NA, grid = NULL, items = NULL) {
  # input validation
  assert_integerish(x = row, lower = 0, upper = nrow(sort), len = 1, null.ok = FALSE)
  assert_integerish(x = column, lower = 0, upper = ncol(sort), len = 1, null.ok = FALSE)
  assert_string(x = item, na.ok = TRUE, null.ok = FALSE)

  # prepare values
  item <- as.character(item)
  if (is.null(grid)) {
    grid <- as_psGrid.psSort(obj = sort)
  }

  # TODO some of this might be better tested in wrappers; would be called too often in here
  # consistency checks

  if (!is.na(item)) {
    # item must not already be placed in sort
    # when used from JS, remember to first clear sending cell, then write to receiving cell, otherwise this fails
    assert_false(x = item %in% sort, na.ok = TRUE)
    # assert_true(x = grid[row, column], na.ok = FALSE, .var.name = paste("row", row, "column", column))
  }

  sort[row, column] <- item
  return(sort)
}
