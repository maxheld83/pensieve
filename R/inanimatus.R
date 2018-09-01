#' @title HTML widget factory for sorting grids
#' @description
#' Creates HTML widgets from sorting grids.
#' Wraps pensieve's inanimatus.js.
#' End users should use the convenient print method wrappers.
#' @inheritParams psGrid
#' @inheritParams htmlwidgets::createWidget
#' @return An htmlwidget.
#' @export
inanimatus <- function(grid = as_psGrid(obj = c(1,2,3,5,3,2,1)),
                       header = TRUE,
                       footer = TRUE,
                       aspect_ratio_cards = 16/9,
                       width = NULL,
                       height = NULL) {
  # dependencies
  requireNamespace2("htmlwidgets")

  # input validation
  grid <- as_psGrid(grid)
  assert_flag(x = header, na.ok = FALSE, null.ok = FALSE)
  assert_flag(x = footer, na.ok = FALSE, null.ok = FALSE)
  assert_scalar(x = aspect_ratio_cards, na.ok = FALSE, null.ok = FALSE)

  # calculate height in css percent of parents for cells/rows (= same)
  rowheight <- 100/ncol(grid)
  rowheight <- rowheight / aspect_ratio_cards
  rowheight <- glue(rowheight, "%")

  # create col and row names when there are none
  if (is.null(colnames(grid))) {
    colnames(grid) <- as.character(1:ncol(grid))
  }
  if (is.null(rownames(grid))) {
    rownames(grid) <- LETTERS[1:nrow(grid)]
  }

  x <- list(grid = grid,
            colnames = colnames(grid),
            rownames = rownames(grid),
            header = header,
            footer = footer,
            aspect_ratio_cards = aspect_ratio_cards)

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
      knitr.figure = TRUE
    )
  )
}
