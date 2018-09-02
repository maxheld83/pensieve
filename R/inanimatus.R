#' @title HTML widget factory for sorting grids
#' @description
#' Creates HTML widgets from sorting grids.
#' Wraps pensieve's inanimatus.js.
#' End users should use the convenient print method wrappers.
#' @inheritParams psGrid
#' @inheritParams htmlwidgets::createWidget
#' @param scale_2_height `[logical()]`
#' giving whether widget should, in addition to width, *also* be scaled to an ancestor height.
#' Only set `TRUE` if you are sure that the surrounding markup passes down a valid css height property, otherwise you will get bad css for the widget.
#' Defaults to `NULL`; `FALSE` when rendering via knitr, otherwise `TRUE`.
#' @return An htmlwidget.
#' @export
inanimatus <- function(grid = as_psGrid(obj = c(1,2,3,5,3,2,1)),
                       header = TRUE,
                       footer = TRUE,
                       aspect_ratio_cards = 16/9,
                       scale_2_height = NULL,
                       width = NULL,
                       height = NULL) {
  # dependencies
  requireNamespace2("htmlwidgets")

  # input validation
  grid <- as_psGrid(grid)
  assert_flag(x = header, na.ok = FALSE, null.ok = FALSE)
  assert_flag(x = footer, na.ok = FALSE, null.ok = FALSE)
  assert_flag(x = scale_2_height, na.ok = FALSE, null.ok = TRUE)
  assert_scalar(x = aspect_ratio_cards, na.ok = FALSE, null.ok = FALSE)

  # create col and row names when there are none
  if (is.null(colnames(grid))) {
    colnames(grid) <- as.character(1:ncol(grid))
  }
  if (is.null(rownames(grid))) {
    rownames(grid) <- LETTERS[1:nrow(grid)]
  }

  # infer scale_2_height from runtime
  if (is.null(scale_2_height)) {
    scale_2_height <- !is_knitr()
  }

  x <- list(
    grid = grid,
    colnames = colnames(grid),
    rownames = rownames(grid),
    header = header,
    footer = footer,
    aspect_ratio_cards = aspect_ratio_cards,
    scale_2_height = scale_2_height
  )

  # create the widget
  htmlwidgets::createWidget(
    name = "inanimatus",
    package = "pensieve",
    x = x,
    width = width,
    height = height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.fill = TRUE,
      browser.padding = "0",
      viewer.padding = "0",
      knitr.figure = FALSE, # this is not a plot!
      knitr.defaultWidth = "100%",
      knitr.defaultHeight = "inherit"
    )
  )
}
