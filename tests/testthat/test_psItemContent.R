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
  checkmate::expect_list(
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
  testthat::expect_warning(
    object = render_items(items = "foo", fontsize = "tiny")
  )
})

test_that(desc = "errors out on language unknown to pandoc", code = {
  skip(message = "in dev")
  testthat::expect_error(
    object = render_items(items = "foo", fontsize = "tiny", lang = "klingon")
  )
})

test_that(desc = "works with all accepted languages", code = {
  skip_on_os(os = "mac") # this just takes too damn long
  for (i in langs) {
    checkmate::expect_list(
      x = {
        render_items(items = "zap", lang = i, fontsize = "tiny")
      },
      info = i
    )
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


context("Converseion from PDF to SVG")

test_that(desc = "conversion from pdf to svg works", code = {
  skip(message = "in development")
  skip_on_os(os = c("windows", "mac"))  # no easy way to get pdf2svg
  pdf_input <- "test1.pdf"
  checkmate::expect_file_exists(x = pdf_input)
  pdf2svg(pdf_input = pdf_input)
  checkmate::expect_file_exists(x = "test1.svg")
})
