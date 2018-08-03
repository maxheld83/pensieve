# setwd(dir = "tests/testthat/") # for interactive testing
setup(code = {
  fs::dir_copy(path = "test_conversions", new_path = "test_conversions_run")
  setwd("test_conversions_run")
})
teardown(code = {
  # comment me out to debug results
  fs::dir_delete(path = "test_conversions_run")
})


# md2latex ====
context("Conversion from markdown to LaTeX")
test_file <- fs::path("test_md2tex", ext = "md")

test_that(desc = "on file system works", code = {
  path_out <- md2tex(path = test_file)
  expect_file(x = path_out, extension = "tex")
})

test_that(desc = "errors out when pandoc is unavailable", code = {
  withr::local_path(new = "", action = "replace")  #  this will kill pandoc
  expect_error(object = md2tex(path = test_file))
})

test_that(desc = "errors out when pandoc times out", code = {
  md <- glue_collapse(x = rep(x = "Repeated often enough, this should choke Pandoc.", times = 1000000), sep = " ")
  readr::write_lines(x = md, path = "choker.md")
  expect_error(md2tex(path = "choker.md"))
})

test_that(desc = "errors out on language unknown to pandoc", code = {
  expect_error(object = md2tex(path = test_file, lang = "klingon"))
})


# texi2pdf ====
context("Compilation from LaTeX to PDF")
test_file <- fs::path("test_texi2pdf2", ext = "tex")

test_that(desc = "on file system works", code = {
  expect_file(x = texi2pdf2(path = test_file), extension = "pdf")
})

test_that(desc = "conversion errors out on invalid LaTeX inside markdown", code = {
  expect_error(object = texi2pdf2(path = "bad.tex"))
})

test_that(desc = "conversion errors out on locally unavailable babel language", code = {
  expect_error(object = suppressWarnings(texi2pdf2(path = "bad_language.tex")))
})

test_that(desc = "unavailable babel languages are properly identified", code = {
  expect_false(object = test_latex_lang(x = "as"))
  expect_true(object = test_latex_lang(x = "en-US"))
})


# pdf2svg ====
context("Conversion from PDF to SVG")
test_file <- fs::path("test_pdf2svg", ext = "pdf")

test_that(desc = "on file system works", code = {
  skip_on_os(os = c("windows"))  # no easy way to get pdf2svg
  expect_file_exists(x = pdf2svg(path = test_file), extension = "svg")
})


# svg2grob ====
context("Conversion from SVG to R graphics (grob)")
test_file <- fs::path("test_svg2grob", ext = "svg")

test_that(desc = "on file system works", code = {
  x <- svg2grob(path = test_file)
  expect_true(object = grid::is.grob(x = x))
})


# test whole chain ====
context("Conversion through the whole chain")
test_file <- fs::path("test_md2tex", ext = "md")

test_that(desc = "works from given md", code = {
  out_path <- md2tex(path = test_file)
  out_path <- texi2pdf2(path = out_path)
  expect_file(x = out_path, extension = "pdf")
  skip_on_os(os = c("windows"))
  out_path <- pdf2svg(path = out_path)
  expect_file(x = out_path, extension = "svg")
  x <- svg2grob(path = out_path)
  expect_true(object = grid::is.grob(x = x))
})


# virtually ====
context("Virtualised conversion")
test_that(desc = "from/to R objects works", code = {
  virt_tex <- md2tex_mem(x = c("bar", "zap"), path_in = "foo.md")
  expect_character(x = virt_tex, any.missing = FALSE, null.ok = FALSE)
  virt_pdf <- texi2pdf2_mem(x = virt_tex, path_in = "foo.tex")
  expect_vector(x = virt_pdf, null.ok = FALSE)
  virt_svg <- pdf2svg_mem(x = virt_pdf, path_in = "foo.pdf")
  expect_vector(x = virt_svg, null.ok = FALSE)
  virt_grob <- svg2grob_mem(x = virt_svg, path_in = "foo.svg")
  expect_vector(x = virt_svg, null.ok = FALSE)
})


# memoised
context("Memoised conversion")
test_that(desc = "is a lot faster", code = {
  skip_on_travis()
  mem_tex <- md2tex_mem(x = c("bar", "zap"), path_in = "foo.md")
  mem_tex_unique <- md2tex_mem(x = sample(x = LETTERS, size = 100, replace = TRUE), path_in = "unique.md")
  first <- system.time(expr = {texi2pdf2_mem(x = mem_tex_unique, path_in = "unique.tex")})
  second <- system.time(expr = {texi2pdf2_mem(x = mem_tex_unique, path_in = "unique.tex")})
  expect_gt(object = first["elapsed"], expected = second["elapsed"] * 10)
  takes_less_than(amount = )
  # expect at least 10fold increase
})
