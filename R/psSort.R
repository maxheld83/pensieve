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
#' `NA` is used for empty *and* disallowed cells (see [psGrid][psGrid]).
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
#' @example tests/testthat/helper_psGrid.R
#' @example tests/testthat/helper_psSort.R
#' @export
psSort <- function(sort, desc_x = NULL, desc_y = NULL, polygon = "rectangle", offset = NULL) {
  # this is DUPLICATE code from psGrid!
  if (is.null(dimnames(sort))) {
    colnames(sort) <- make_rank_names(max_rank = ncol(sort))
  }
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
validate_S3.psSort <- function(x, grid = NULL, items = NULL, ...) {
  # psSort has mostly the same validation on x as psGrid;
  # to avoid duplication, we here use this somewhat hacky trick
  assert_S3(as_psGrid(x), collection = ps_coll, var.name = "sort")
  # IF desc_x/desc_y are given ensure that they are proper
  walk(.x = c("desc_x", "desc_y"), .f = function(x) {
    assert_character(
      x = x %@% x,
      any.missing = FALSE,
      len = 1,
      null.ok = TRUE,
      add = ps_coll,
      .var.name = "sort")
  })

  # check x VS grid
  # check if sort rank corresponds to grid rank
  if (!is.null(grid)) {
    assert_S3(x = grid, collection = ps_coll, var.name = "grid")
    assert_matrix(
      x = grid,
      nrows = nrow(x),
      ncols = ncol(x),
      add = ps_coll,
      .var.name = "grid"
    )
  }


  # check x VS items
  # check that there are enough cells for all items
  # this is pretty strict, but recall that this is methodologically necessary:
  # dropping some items would imply that the ipsative comparison is no longer the same
  if (!is.null(items)) {
    assert_S3(items, collection = ps_coll, var.name = "items")
    assert_vector(x = items, max.len = length(x), add = ps_coll, .var.name = "items")
  }


  # check per cell and per row
  dirty_sort <- x
  # cannot use simple subsetting method here, because that would trigger tests already
  clean_sort <- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
  attributes(clean_sort) <- attributes(x)

  inset_psSort(
    x = clean_sort,
    value = dirty_sort,
    grid = grid,
    items = items
  )

  NextMethod(ps_coll = ps_coll)
}


#' @title Place item into rows and columns of a closed sort.
#' @description
#' This function accepts vectors and names for i and j, much like `[<-`.
#' Below inset_psSort1 accepts only a single cell as input
#' @inheritParams psSort
#' @inheritParams base::Extract
#' @return A matrix of class `psSort`.
#' @noRd
#TODO this should ideally be an internal generic method, blocked by https://github.com/maxheld83/pensieve/issues/421
inset_psSort <- function(x, i = NULL, j = NULL, value = NA, grid = NULL, items = NULL) {
  # null indices means inset *all*
  if (is.null(i)) {
    i <- 1:nrow(x)
  }
  if (is.null(j)) {
    j <- 1:ncol(x)
  }

  # nested for loops are bad, yes, but
  # - purrr does not support matrices
  # - apply makes this harder to read
  for (row in i) {
    for (column in j) {
      x[i, j] <- inset_psSort1(
        x = x,
        i = row,
        j = column,
        value = value[row, column],
        grid = grid,
        items = items
      )
    }
  }
}
inset_psSort1 <- function(x, i, j, value = NA, grid = NULL, items = NULL) {
  sort <- x
  row <- i
  column <- j
  item <- value
  # input validation
  assert_atomic_vector(
    x = row,
    any.missing = FALSE,
    all.missing = FALSE,
    min.len = 1,
    max.len = nrow(sort),
    .var.name = "row"
  )
  assert_atomic_vector(
    x = column,
    any.missing = FALSE,
    all.missing = FALSE,
    min.len = 1,
    max.len = ncol(sort),
    .var.name = "column"
  )
  assert_string(x = item, na.ok = TRUE, null.ok = FALSE)

  if (!is.na(item)) {

    # item must not already be placed in sort
    if (item %in% sort) {
      # when used from JS, remember to first clear sending cell, then write to receiving cell, otherwise this fails
      pos <- which(sort == item, arr.ind = TRUE, useNames = TRUE)
      stop(
        glue(
          "Items must be unique in a sort.
           Item {item} is already in the sort at row {pos[,'row']} and column {pos[,'col']}."
        ),
        call. = FALSE
      )
    }

    # item target position must be allowed as per grid
    if (!is.null(grid)) {  # we only test this if we actually *have* a grid, otherwise pointless
      if (!grid[row, column]) {
        stop(
          glue(
            "Item {item} cannot be placed into cell at row {row} and column {column}.
               Cell is 'FALSE' in 'grid' and must therefore remain empty."
          ),
          call. = FALSE
        )
      }
    }

    # item must be one of items
    if (!is.null(items)) {  # we only test this if we actually *have* items, otherwise pointless
      assert_choice(x = item, choices = items, null.ok = FALSE)
    }
  }

  x[row, column] <- value
  x
}


# coercion ====
#' @rdname psSort
#' @param obj
#' An object which can be coerced to a character matrix of class [psSort][psSort], currently one of:
#' - a (named) integer(ish) vector, giving the x-axis item ranks per item (names are retained as item handles).
#' @export
as_psSort <- function(obj, ...) {
  UseMethod("as_psSort")
}
as_psSort.default <- function(obj, ...) {
  stop_coercion(obj = obj, target_class = "psSort")
}
as_psSort.psSort <- function(obj, ...) {
  assert_S3(x = obj)
  obj
}

#' @describeIn psSort Coercion from [psGrid][psGrid] (sets all to `NA`)
#' @export
as_psSort.psGrid <- function(obj, ...) {
  assert_S3(obj)

  sort <- matrix(data = NA, nrow = nrow(obj), ncol = ncol(obj))
  storage.mode(x = sort) <- "character"
  dimnames(sort) <- dimnames(obj)
  psSort(
    sort = sort,
    polygon = obj %@% "polygon",
    offset = obj %@% "offset"
  )
}

#' @describeIn psSort Coercion from other vector forms
#' @export
as_psSort.integer <- function(obj, grid = NULL, ...) {
  # input validation
  assert_integer(x = obj, any.missing = TRUE)

  # infer grid
  if (!is.null(grid)) {
    grid <- as_psGrid(obj = unclass(table(obj)), ...)
    # ... pass on polygon and offset
  }
  assert_S3(grid)

  NULL
}

#' @rdname psSort
#' @export
as_psSort.numeric <- function(obj, grid = NULL, ...) {
  if (test_integerish(x = obj)) {
    as_psSort(obj = as.integer(obj), ...)
  } else {
    # TODO also offer method for numerics, such as z-scores
    NextMethod()
  }
}
