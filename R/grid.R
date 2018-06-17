# html5 ====

html5_grid <- function(grid, browsable = TRUE, header = TRUE, footer = TRUE) {
  # input verification
  assert_flag(x = browsable, na.ok = FALSE, null.ok = FALSE)
  assert_flag(x = header, na.ok = FALSE, null.ok = FALSE)
  assert_flag(x = footer, na.ok = FALSE, null.ok = FALSE)

  # test dependencies
  requireNamespace2("htmltools")

  # gather HTML5 dependencies
  bs <- htmltools::htmlDependency(
    name = "bootstrap",
    version = "3.3.7",
    src = "inst/bootstrap-3.3.7-dist/",
    stylesheet = "css/bootstrap.min.css",
    all_files = TRUE,
    package = "pensieve"
  )
  html5_grid_style <- htmltools::htmlDependency(
    name = "html5_grid",
    version = "0.0.9999",
    src = "inst/",
    stylesheet = "html5_grid.css",
    all_files = TRUE,
    package = "pensieve"
  )

  # create output
  output <- htmltools::tagList(
    # dependencies
    bs,
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
          htmltools::tags$tr(html5_grid_row(rowvec = grid[rname,]))
        })
      )
    )
  )

  # return output
  if (browsable) {
    browsable(output)
  } else {
    output
  }
}

html5_grid_row <- function(rowvec) {
  purrr::map(.x = rowvec, .f = function(cell) {
    cellout <- htmltools::tags$td(cell, class = "cell")
    if (cell == TRUE) {
      cellout <- htmltools::tagAppendAttributes(tag = cellout, class = "allowed")
    }
    return(cellout)
  })
}
