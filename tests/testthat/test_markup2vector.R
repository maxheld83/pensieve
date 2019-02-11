skip(message = "Skipping because currently no new pandoc on verse image.")
context("Conversions")
skip_on_dev_machine()
skip_on_appveyor()  # no latex, no pandoc, no pdf2svg, no nothin'
skip_on_cran()  # too expensive

# setwd(dir = "tests/testthat/") # for interactive testing
setup(code = {
  fs::dir_copy(path = "test_markup2vector", new_path = "test_markup2vector_run")
  setwd("test_markup2vector_run")
})
teardown(code = {
  # comment me out to debug results
  fs::dir_delete(path = "test_markup2vector_run")
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
  virt_tex <- md2tex_mem(x = c("bar", "zap"))
  expect_character(x = virt_tex, any.missing = FALSE, null.ok = FALSE)
  virt_pdf <- texi2pdf2_mem(x = virt_tex)
  expect_vector(x = virt_pdf, null.ok = FALSE)
  skip_on_os(os = c("windows"))
  virt_svg <- pdf2svg_mem(x = virt_pdf)
  expect_vector(x = virt_svg, null.ok = FALSE)
  virt_grob <- svg2grob_mem(x = virt_svg)
  expect_vector(x = virt_svg, null.ok = FALSE)
})


# memoised ====
context("Memoised conversion")
test_that(desc = "is a lot faster", code = {
  mem_tex_unique <- md2tex_mem(x = sample(x = LETTERS, size = 100, replace = TRUE))
  first <- system.time(expr = {texi2pdf2_mem(x = mem_tex_unique)})
  second <- system.time(expr = {texi2pdf2_mem(x = mem_tex_unique)})
  expect_gt(object = first["elapsed"], expected = second["elapsed"] * 5) # expect at least 5 fold increase
})


# render chain ====
context("Render chain")
test_that(desc = "converts whole chain", code = {
  l <- list(foo = "foo", bar = "bar")
  target_types = c("character", "raw", "raw", "list")
  walk2(
    .x = names(render_chain_formats),
    .y = target_types,
    .f = function(target_format, target_type) {
      expect_list(
        x = render_chain(l = l, format = target_format),
        types = target_type,
        any.missing = FALSE,
        len = 2,
        unique = TRUE,
        null.ok = FALSE,
        info = target_format)
    }
  )
})


# find fontsize ====
context("find fontsize")
test_that(desc = "single markdown vector: finds largest possible fontsize to stay on one page", code = {
  little_text_res <- find_fontsize(l = list("A"))
  little_text_i <- which(fontsizes_local == little_text_res)  # this is the index
  expect_choice(x = little_text_res, choices = fontsizes_local)
  much_text <- rep(x = LETTERS, times = 30)
  much_text_res <- find_fontsize(l = list(much_text))
  expect_choice(x = much_text_res, choices = fontsizes_local)
  much_text_i <- which(fontsizes_local == much_text_res)
  expect_gt(object = little_text_i, expected = much_text_i)
})
test_that(desc = "list of markdown vectors: finds largest possible fontsize to stay on one page across all list elements", code = {
  l <- list(medium = rep("medium", times = 200), short = "short", long = rep("long", times = 500))
  res_all <- find_fontsize(l = l)
  expect_choice(x = res_all, choices = fontsizes_local)
  res_ms <- find_fontsize(l = l[c("medium", "short")])
  res_sl <- find_fontsize(l = l[c("short", "long")])
  res_ms_i <- which(fontsizes_local == res_ms)
  res_sl_i <- which(fontsizes_local == res_sl)
  expect_gt(object = res_ms_i, expected = res_sl_i)
  expect_equal(object = find_fontsize(l = l[c("short", "medium", "long")]), expected = res_all)
})
test_that(desc = "works with incorrectly ordered fontsizes", code = {
  expect_equal(
    object = find_fontsize(l = list("A"), fontsizes_local_possible = fontsizes_local[c(7:4, 1:2)]),
    expected = find_fontsize(l = list("A"), fontsizes_local_possible = fontsizes_local[c(1:2, 4:7)])
  )
})
