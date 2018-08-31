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
    height = height)
}
