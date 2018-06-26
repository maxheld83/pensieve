# html5 ====

#' @title Write HTML5 markup for grid.
#' @description This is a worker function to write out grids as HTML5 markup with dependencies.
#' @param grid A logical matrix, giving the sorting grid, indicating whether the cells are allowed or not.
#' @inheritParams psGrid
#' @return An [htmltools::tagList()].
#' @noRd
html5_grid <- function(grid, header, footer, aspect_ratio_cards, ...) {
  # test dependencies
  requireNamespace2("htmltools")

  # input verification
  assert_flag(x = header, na.ok = FALSE, null.ok = FALSE)
  assert_flag(x = footer, na.ok = FALSE, null.ok = FALSE)
  assert_scalar(x = aspect_ratio_cards, na.ok = FALSE, null.ok = FALSE)

  # calculate height in css percent of parents for cells/rows (= same)
  rowheight <- 100/ncol(grid)
  rowheight <- rowheight / aspect_ratio_cards
  rowheight <- glue(rowheight, "%")

  # gather HTML5 dependencies
  bs <- htmltools::htmlDependency(
    name = "bootstrap",
    version = "3.3.7",
    src = pensieve_system_file("bootstrap-3.3.7-dist"),
    stylesheet = "css/bootstrap.min.css",
    all_files = TRUE
  )
  jquery <- htmltools::htmlDependency(
    name = "jquery",
    version = "1.12.4",
    src = pensieve_system_file("jQuery"),
    script = "jquery-1.12.4.js",
    all_files = FALSE
  )
  html5_grid_style <- htmltools::htmlDependency(
    name = "html5_grid",
    version = "0.0.9999",
    src = pensieve_system_file("html5_grid"),
    stylesheet = "html5_grid.css",
    # we can't programmatically (easily) add the calculated padding-top to this .css, so we add it below in head.
    # so NOTICE: this css is actually incomplete
    all_files = TRUE,
    script = "html5_grid.js",
    # add programmatically calculated padding-top to hack-fix row height
    # must be place here so that is placed in the head even when called in knitr
    head = htmltools::doRenderTags(
      htmltools::tags$style(
        glue::glue(".ps-grid ", ".cell ", "{{", htmltools::css(`padding-top` = rowheight), "}}")
      )
    )
  )

  # create output
  output <- htmltools::tagList(
    # dependencies
    bs,
    jquery,
    html5_grid_style,

    htmltools::tags$table(
      class = "ps-grid",

      # header
      if (header) {
        htmltools::tags$thead(
          htmltools::tags$tr(
            purrr::map(.x = colnames(grid), .f = function(cname) {
              htmltools::tags$th(cname)
            })
          )
        )
      },

      # footer
      if (footer) {
        htmltools::tags$tfoot(
          htmltools::tags$tr(
            purrr::map(.x = colnames(grid), .f = function(cname) {
              htmltools::tags$td(cname)
            })
          )
        )
      },

      # body
      htmltools::tags$tbody(
        purrr::map(.x = rownames(grid), .f = function(rname) {
          htmltools::tags$tr(
            html5_grid_row(rowvec = grid[rname,], ...)
          )
        })
      )
    )
  )

  return(output)
}

#' @title Write HTML grid for a single row of cells
#' @param rowvec A logical vector giving the availability of cells.
#' @return An [htmltools::tagList()].
#' @noRd
html5_grid_row <- function(rowvec, ...) {
  purrr::map(.x = rowvec, .f = function(cell) {
    html5_grid_cell(allowed = cell, ...)
  })
}

#' @title Write HTML for single cell
#' @param allowed A logical flag whether cell is available
#' @noRd
html5_grid_cell <- function(allowed = TRUE, ...) {
  # input validation
  assert_flag(x = allowed, na.ok = FALSE, null.ok = FALSE)

  if (allowed) {
    cell <- htmltools::tags$td(
      class = "cell allowed",
      # style = "padding-top: 80px;",
      htmltools::tags$div(
        class = "content",
        html5_grid_cell_content(...)
      )
    )
  } else {
    cell <- htmltools::tags$td(class = "cell")
  }
  return(cell)
}
#' @title Write HTML for single cell content
#' @param type A string, must be one of:
#' - `template` for empty template,
#' - `input` for input fields.
#' @noRd
html5_grid_cell_content <- function(type = "template") {
  # input validation
  assert_string(x = type, na.ok = FALSE, null.ok = FALSE)
  assert_choice(x = type, choices = c("template", "input"))

  switch(
    EXPR = type,
    template = {
      cell_content <- NULL
    },
    input = {
      cell_content <- htmltools::tags$input(type = "text", name = "foo", value = "Pixie")
    }
    # add other types here
  )

  return(cell_content)
}

