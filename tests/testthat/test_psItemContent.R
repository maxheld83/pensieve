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


context("Rendering text items")

test_that(desc = "text gets converted to LaTeX", code = {
  checkmate::expect_list(
    x = rendered_items$tex,
    types = "character",
    any.missing = FALSE,
    len = length(items_text_en),
    unique = TRUE,
    names = "strict",
    null.ok = FALSE)
})

test_that(desc = "conversion from pdf to svg works", code = {
  skip(message = "in development")
  skip_on_os(os = c("windows", "mac"))  # no easy way to get pdf2svg
  pdf_input <- "test1.pdf"
  checkmate::expect_file_exists(x = pdf_input)
  pdf2svg(pdf_input = pdf_input)
  checkmate::expect_file_exists(x = "test1.svg")
})

test_that(desc = "pdf card is produced from string", code = {
  skip_on_appveyor()  # does not have latex
  skip_on_os(os = c("mac"))
  skip(message = "currently in dev")
  output <- pensieve:::make_cards(item_text = "foo", item_handle = "foo_handle")
  checkmate::expect_file_exists(x = output$paths$pdf)
})

test_that(desc = "pandoc works with all accepted languages", code = {

})
