# md2latex ====
context("Conversion from markdown to LaTeX")

# setwd(dir = "tests/testthat/") # for interactive testing

test_that(desc = "works", code = {
  path_out <- md2tex(path = "test.md")
  expect_file(x = path_out, extension = "tex")
})

test_that(desc = "errors out when pandoc is unavailable", code = {
  withr::local_path(new = "", action = "replace")  #  this will kill pandoc
  expect_error(object = md2tex(path = "test.md"))
})

test_that(desc = "errors out when pandoc times out", code = {
  md <- glue_collapse(x = rep(x = "Repeated often enough, this should choke Pandoc.", times = 1000000), sep = " ")
  readr::write_lines(x = md, path = "choker.md")
  expect_error(md2tex(path = "choker.md"))
})

test_that(desc = "errors out on language unknown to pandoc", code = {
  expect_error(object = md2tex(path = "test.md", lang = "klingon"))
})

test_that(desc = "works with all accepted global fontsizes", code = {
  purrr::iwalk(.x = fontsizes_global, .f = function(x, y) {
    expect_file(x = md2tex(path = "test.md", fontsize_global = x), info = y, extension = "tex")
  })
})

test_that(desc = "works with all accepted languages", code = {
  purrr::iwalk(.x = langs, .f = function(x, y) {
    expect_file(x = md2tex(path = "test.md", lang = x), info = y, extension = "tex")
  })
})


# texi2pdf ====
context("Compilation from LaTeX to PDF")

test_that(desc = "works", code = {
  expect_file(x = texi2pdf2(path = "test.tex"), extension = "pdf")
})

test_that(desc = "conversion errors out on invalid LaTeX inside markdown", code = {
  readr::write_lines(c("This is some totally invalid tex", "\\usepackage{"), path = "bad.tex")
  expect_error(object = texi2pdf2(path = "bad.tex"))
})

# pdf2svg ====
context("Conversion from PDF to SVG")

test_that(desc = "works", code = {
  skip_on_os(os = c("windows"))  # no easy way to get pdf2svg
  expect_file_exists(x = pdf2svg(path = "test.pdf"))
})

# svg2grob ====
context("Conversion from SVG to R graphics (grob)")

test_that(desc = "works", code = {

  x <- svg2grob(path = "test.svg")
  expect_true(object = grid::is.grob(x = x))
})
