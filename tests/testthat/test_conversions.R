# md2latex ====
context("Conversion from markdown to LaTeX")

# setwd(dir = "tests/testthat/") # for interactive testing

test_that(desc = "on file system works", code = {
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
  teardown(code = {fs::file_delete("choker.md")})
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
    setup(code = {
      # fs::file_copy(path = "test.tex", new_path = "test-backup.tex", overwrite = TRUE)
    })
    fs::file_copy(path = "test.tex", new_path = "test-backup.tex", overwrite = TRUE)
    expect_file(x = md2tex(path = "test.md", lang = x), info = y, extension = "tex")
    # teardown(code = {
      fs::file_copy(path = "test-backup.tex", new_path = "test.tex", overwrite = TRUE)
      fs::file_delete(path = "test-backup.tex")
    # })
  })
})

# texi2pdf ====
context("Compilation from LaTeX to PDF")

test_that(desc = "on file system works", code = {
  setup(code = {
    fs::file_copy(path = "test.pdf", new_path = "test-backup.pdf", overwrite = TRUE)
    # necessary for cleanup, see below
  })
  expect_file(x = "test.tex", access = "r")
  expect_file(x = texi2pdf2(path = "test.tex"), extension = "pdf")
  processx::run(command = "pdflatex", args = c("--version"))
  teardown(code = {
    fs::file_copy(path = "test-backup.pdf", new_path = "test.pdf", overwrite = TRUE)
    fs::file_delete(path = "test-backup.pdf")
  })
})

test_that(desc = "conversion errors out on invalid LaTeX inside markdown", code = {
  readr::write_lines(c("This is some totally invalid tex", "\\usepackage{"), path = "bad.tex")
  expect_error(object = texi2pdf2(path = "bad.tex"))
  teardown(code = {
    fs::file_delete("bad.tex")
  })
})

# pdf2svg ====
context("Conversion from PDF to SVG")

test_that(desc = "on file system works", code = {
  skip_on_os(os = c("windows"))  # no easy way to get pdf2svg
  expect_file_exists(x = pdf2svg(path = "test.pdf"))
})

# svg2grob ====
context("Conversion from SVG to R graphics (grob)")

test_that(desc = "on file system works", code = {

  x <- svg2grob(path = "test.svg")
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
  mem_tex <- md2tex_mem(x = c("bar", "zap"), path_in = "foo.md")
  mem_tex_unique <- md2tex_mem(x = sample(x = LETTERS, size = 100, replace = TRUE), path_in = "unique.md")
  first <- system.time(expr = {texi2pdf2_mem(x = mem_tex_unique, path_in = "unique.tex")})
  second <- system.time(expr = {texi2pdf2_mem(x = mem_tex_unique, path_in = "unique.tex")})
  expect_gt(object = first["elapsed"], expected = second["elapsed"] * 10)
  # expect at least 10fold increase
})
