# helper ====
#' @title Store sorting grid as logical matrix.
#'
#' @description
#' Stores sorting grid as a logical \eqn{i * k} matrix with sorting columns as columns, sorting rows as rows and `TRUE` (allowed) or `FALSE` (not allowed) in cells.
#'
#' @details
#' *Every* sort must have a grid.
#' Even a free distribution must have a grid, giving the maximum indices of rows and columns, but with all cells `TRUE`.
#'
# this argument is almost the same as sort for psSort; some duplication
#' @param grid `[matrix()]`
#' giving the availability of cells for sorting as `logical(1)` values.
#' The (horizontal) x-axis is assumed to be the sorting direction, the (vertical) y-axis for recording ties.
#' Dimensions can be named (recommended), giving a short description of the sorting dimension (only applicable to the x-axis).
#' Row and column *indeces* can also be named, but names are purely cosmetic.
#'
#' @eval document_choice_arg(arg_name = "polygon", choices = polygons, before = "giving polygon to use for tiling", default = "rectangle")
#'
#' @eval document_choice_arg(arg_name = "offset", choices = offsets, before = "giving the *rows* to be offset.", default = "null", null = "in which case no offset is used for a square tiling.")
#'
#' @section Hexagonal tiling:
#' [psGrid][psGrid] and [psSort][psSort] store *all* sorting grids as *rectangular* matrices, using what is known as the ["offset" notation for hexagonal tiling](https://www.redblobgames.com/grids/hexagons/).
#' In offset notation, hexagonal tilings are saved as if they were normal (square) tilings, with an additional attribute giving which rows are to be offset.
#' In this way, both square and hexagonal tilings can be stored in a similar format.
#' They are also intuitive to use, where the outer limits of the tiling are rectangular, and rotation is not required, both of which are usually the case for sorting.
#' However, linear algebra operations are no longer defined on such hexagonal matrices in offset notation (that would require cube or axial coordinates).
#' Remember not to run such operations on hexagonally tiled [psGrid][psGrid]s or [psSort][psSort]s.
# TODO if we ever need "proper" cube/axial coordinates, say for a real honeycomb structure, this should be implemented as a subclass to `psGrid`, with appropriate coercion methods. Seems overkill for now.
#'
#' The `offset` argument is used to switch between loosely defined tiling patterns.
#' Strictly speaking there are  *three regular* tiling patterns: square, hexagonal and triangular.
#' However, items are more easily typeset in *rectangles* than in squares, hexagons or triangles.
#' You can therefore also use "square" tiling (`offset = NULL`) for rectangulary set items, and even "hexagonal" tiling (`offset = "even"` or `offset = "odd"`) for rectangles (in a "brickwall" pattern) and irregular (stretched or squeezed) hexagons.
#' One combination remains impossible: you cannot have "square" tiling (`offset = NULL`) with hexagons (`polygon = 'hexagon'`).
#'
#' The aspect ratio of the *irregular* polygons is currently only provided to respective `knit_print()` methods.
#' To achieve *regular* square and hexagonal tiling (though this is unlikely to be useful), set `aspect_ratio_cards` to `1`.
#'
#' Notice that `offset` always refers to *rows*, and as such implies hexagonal tiling in "**pointy**"-topped rotation.
# TODO in the astronomically unlikely case of flat topped hex grids, this would need to be another variable; offset would then refer to *columns*
#'
#' Remember that rows for `offset` are given using *R* indices, starting with `1`.
#' Examples of offset notation in most other programming languages will differ."
#'
#' @example tests/testthat/helper_psGrid.R
#'
#' @family S3 classes from `pensieve`
#'
#' @return A logical matrix of class [psGrid][psGrid].
#'
#' @export
psGrid <- function(grid,
                   polygon = "rectangle",
                   offset = NULL) {
  grid <- new_psGrid(grid = grid, polygon = polygon, offset = offset)
  assert_S3(grid)
  return(grid)
}
offsets <- c("even", "odd")
polygons <- c("rectangle", "hexagon")

# constructor
new_psGrid <- function(grid, polygon, offset) {
  # assert base type
  assert_matrix(
    x = grid,
    mode = "logical",
    any.missing = FALSE,
    all.missing = FALSE,
    null.ok = FALSE
  )

  structure(
    .Data = grid,
    polygon = polygon,
    offset = offset,
    class = c("psGrid", "matrix")
  )
}

#' @describeIn psGrid Validation
#' @inheritParams validate_S3
#' @export
validate_S3.psGrid <- function(x, ...) {
  map(.x = list(colnames(x), rownames(x)), .f = function(x) {
    assert_names2(
      x = x,
      type = "unique",
      add = ps_coll,
      .var.name = "grid"
    )
  })

  assert_choice(
    x = attr(x = x, which = "polygon"),
    choices = polygons,
    null.ok = FALSE,
    .var.name = "grid",
    add = ps_coll
  )
  assert_choice(
    x = attr(x = x, which = "offset"),
    choices = offsets,
    null.ok = TRUE,
    .var.name = "grid",
    add = ps_coll
  )

  NextMethod(ps_coll = ps_coll)
}

# coercion ====
#' @rdname psGrid
#' @param obj
#' An object which can be coerced to a logical matrix of class [psGrid][psGrid].
#' @export
as_psGrid <- function(obj, ...) {
  UseMethod("as_psGrid")
}
as_psGrid.default <- function(obj, ...) {
  stop_coercion(obj = obj, target_class = "psGrid")
}
as_psGrid.psGrid <- function(obj, ...) {
  assert_S3(x = obj)
  obj
}
#' @describeIn psGrid Coercion from (named) integer(ish) vector, giving the column height of `TRUE`s from the bottom (names are retained as column names).
#' @export
as_psGrid.integer <- function(obj, ...) {
  # input validation
  assert_integer(x = obj, lower = 0, any.missing = FALSE, null.ok = FALSE)

  overall_height <- max(obj)

  # purrr isn't good for this job because it only returns tibbles; overkill here
  m <- sapply(X = obj, FUN = function(this_height) {
    this_column <- c(
      rep(FALSE, overall_height - this_height),
      rep(TRUE, this_height)
    )
    return(this_column)
  })

  if (is.null(names(obj))) {
    # this is necessary, because below syntax otherwise yields an empty list
    dim_names <- NULL
  } else {
    # here, as always, we assume that x is the used dimension
    dim_names <- list(c(NULL), names(obj))
  }
  m <- matrix(
    data = m,
    nrow = max(obj),
    ncol = length(obj),
    dimnames = dim_names
  )

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
#' @describeIn psGrid Coercion from a logical matrix as per [psGrid][psGrid].
#' @export
as_psGrid.matrix <- function(obj, ...) {
  psGrid(grid = obj, ...)
}
#' @describeIn psGrid Coercion from a character matrix [psSort][psSort] (sets all cells to `TRUE`).
#' @export
as_psGrid.psSort <- function(obj, ...) {
  grid <- matrix(data = TRUE, nrow = nrow(obj), ncol = ncol(obj))
  dimnames(grid) <- dimnames(obj)
  psGrid(
    grid = grid,
    polygon = obj %@% "polygon",
    offset = obj %@% "offset"
  )
}

# print ====
#' @describeIn psGrid Printing inside knitr chunks
#' @param header,footer A logical flag, defaults to `TRUE`, in which case column or row names from `grid` are included as headers or footers, respectively.
#' Missing row or column names are replaced with sensible defaults.
#' @param aspect_ratio_cards
#' A numeric scalar, giving width divided by height of *individual cards* (such as 16/9 for screen dimensions).
#' Aspect ratio of *cards* is required to appropriately set the resulting dimensions of the *grid*.
#' Defaults to standard business cards.
#' @template knit_print
#' @export
knit_print.psGrid <- function(x,
                              header = TRUE,
                              footer = TRUE,
                              aspect_ratio_cards = 85/54,
                              inline = FALSE,
                              ...) {
  if (is.null(colnames(x))) {
    colnames(x) <- make_pos_names(max_pos = ncol(x))
  }
  if (is.null(rownames(x))) {
    rownames(x) <- LETTERS[1:nrow(x)]
  }
  if (!is.null(attr(x = x, which = "offset"))) {
    stop("Sorry, do not know how to print non-square tiled grids.
         If you need this feature, contact the package author.")
  }
  if (inline) {
    # makes no sense / is complicated to print html5_grid inline, so we pass on to default knit_print method for matrix
    NextMethod()
  } else {
    if (knitr::is_html_output()) {
      res <- inanimatus(
        grid = x,
        header = header,
        footer = footer,
        aspect_ratio_cards = aspect_ratio_cards
      )
      knit_print(res, ...)
    } else {
      # no special idea about this format, so pass it on
      NextMethod()
    }
  }
}

#' @title Make sorting position names for sorting grids
#' @description
#' These are just cosmetic names.
#' @param max_pos `[integer(1)]`
#' @return `[character()]`
#' @noRd
make_pos_names <- function(max_pos) {
  if (is_even(max_pos)) {
    # even rows make sense only for unipolar sorts really
    # this is just accepted here, and we do not store for now whether sorts are unipolar or bipolar
    return(as.character(1:max_pos))
  }
  extreme <- max_pos %/% 2
  return(as.character(-extreme:extreme))
}
