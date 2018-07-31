context("psItemContent class")

# object construction
test_that(desc = "construction of text item works", code = {
  expect_s3_class(object = items_text_en, class = c(
    "psItemContentText",
    "psItemContent",
    "character"
  ))
  expect_character(x = items_text_de, len = 2)
  expect_subset(
    x = c("lang"),
    choices = names(attributes(items_text_en))
  )
  expect_named(
    object = items_text_en,
    expected = c("live_2_work", "work_2_live"),
    ignore.order = FALSE,
    ignore.case = FALSE
  )
})

test_that(desc = "construction of image item works", code = {
  expect_s3_class(object = items_image, class = c(
    "psItemContentImage",
    "psItemContent",
    "character"
  ))
  expect_subset(
    x = c("img_dir"),
    choices = names(attributes(items_image))
  )
})


# md2latex ====
context("Conversion from markdown to LaTeX")

test_that(desc = "just works", code = {
  expect_list(
    x = rendered_items$tex,
    types = "character",
    any.missing = FALSE,
    len = length(items_text_en),
    unique = TRUE,
    null.ok = FALSE)
  expect_names2(x = names(rendered_items$tex))
})

test_that(desc = "skips and warns when pandoc is unavailable", code = {
  withr::local_path(new = "", action = "replace")  #  this will kill pandoc
  expect_warning(
    object = render_items(items = "foo")
  )
})

test_that(desc = "errors out when pandoc times out", code = {
  md <- glue_collapse(x = rep(x = "Repeated often enough, this should choke Pandoc.", times = 1000000), sep = " ")
  expect_error(
    object = md2tex(md = md)
  )
})

test_that(desc = "errors out on language unknown to pandoc", code = {
  expect_error(
    object = md2tex(md = "foo", lang = "klingon")
  )
})

test_that(desc = "works with all accepted local fontsizes", code = {
  for (i in fontsizes_local) {
    res <- render_items(items = c("zap", "bar"), fontsize_local = i)
    checkmate::expect_list(x = res, info = i)
  }
})

test_that(desc = "works with all accepted global fontsizes", code = {
  for (i in fontsizes_global) {
    res <- render_items(items = c("zap", "bar"), fontsize_global = i)
    checkmate::expect_list(x = res, info = i)
  }
})

test_that(desc = "works with all accepted alignments", code = {
  for (i in alignments) {
    res <- render_items(items = c("zap", "bar"), alignment = i)
    checkmate::expect_list(x = res, info = i)
  }
})

test_that(desc = "works with all accepted languages", code = {
  for (i in langs) {
    res <- render_items(items = c("zap", "bar"), lang = i)
    checkmate::expect_list(x = res, info = i)
  }
})

test_that(desc = "accepts by-hand LaTeX to override", code = {
  skip(message = "in dev")
  expect_equivalent(object = from_by_hand_latex$tex[[1]], expected = by_hand_latex)
})

context("Compilation from LaTeX to PDF")

test_that(desc = "conversion errors out on invalid LaTeX inside markdown", code = {
  skip(message = "in dev")
  testthat::expect_error(
    object = render_items(items = "\\usepackage{", fontsize = "tiny")
  )
})

test_that(desc = "pdf card is produced from string", code = {
  skip_on_appveyor()  # does not have latex
  skip_on_os(os = c("mac"))
  skip(message = "currently in dev")
  output <- pensieve:::make_cards(item_text = "foo", item_handle = "foo_handle")
  checkmate::expect_file_exists(x = output$paths$pdf)
})


context("Conversion from PDF to SVG")

test_that(desc = "conversion from pdf to svg works", code = {
  skip(message = "in development")
  skip_on_os(os = c("windows", "mac"))  # no easy way to get pdf2svg
  pdf_input <- "test1.pdf"
  checkmate::expect_file_exists(x = pdf_input)
  pdf2svg(pdf_input = pdf_input)
  checkmate::expect_file_exists(x = "test1.svg")
})
