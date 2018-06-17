# html5 grid ====

write_html5grid <- function(grid, browsable = TRUE) {
  # input verification
  assert_flag(x = browsable, na.ok = FALSE, null.ok = FALSE)

  # test dependencies
  requireNamespace2("htmltools")

  # gather HTML5 dependencies
  dep <- htmltools::htmlDependency(
    name = "bootstrap",
    version = "3.3.7",
    src = "inst/bootstrap-3.3.7-dist/",
    stylesheet = "css/bootstrap.min.css",
    all_files = TRUE,
    package = "pensieve"
  )

  # create output
  output <- htmltools::tagList(
    dep,
    htmltools::tags$table(
      purrr::map(.x = rownames(grid), .f = function(rname) {
        htmltools::tags$tr(write_html5grid_row(rowvec = grid[rname,]))
      })
    )
  )

  # return output
  if (browsable) {
    browsable(output)
  } else {
    output
  }
}

write_html5grid_row <- function(rowvec) {
  purrr::map(.x = rowvec, .f = function(cell) {
    htmltools::tags$td(cell)
  })
}
