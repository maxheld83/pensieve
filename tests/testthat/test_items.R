context("Constructing items")

# object construction ====
test_that(desc = "construction of monolingual text item works", code = {
  monolingual_text <- itemConcourse(
    concourse = c(live_2_work = "Man lives to work.",
                  work_2_live = "Man works to live."),
    languages = c("english"),
    type = "textItem",
    markup = "plain"
  )
  expect_s3_class(object = monolingual_text, class = c("textItem", "itemConcourse"))
})
test_that(desc = "construction of monolingual image item works", code = {
  monolingual_image <- itemConcourse(
    concourse = c(peach = "peach.jpg",
                  pear = "pear.jpg"),
    languages = c("english"),
    img_dir = file.path(system.file(package = "pensieve"), "extdata", "fruit"),
    # these files ship with pensieve
    type = "imageItem"
  )
  expect_s3_class(object = monolingual_image, class = c("imageItem", "itemConcourse"))
})

test_that(desc = "construction of multilingual text item works", code = {
  # TODO avoid this duplication, this is copied from examples
  multilingual_text <- itemConcourse(
    concourse = matrix(
      data = c(
        "Man lives to work.", "Man lebt, um zu arbeiten.",
        "Man works to live.", "Man arbeitet, um zu leben."
      ),
      nrow = 2,
      ncol = 2,
      dimnames = list(items = c("live_2_work", "work_2_live"))
    ),
    languages = c("english", "ngerman"),
    type = "textItem",
    markup = "plain"
  )
  expect_s3_class(object = multilingual_text, class = c("textItem", "itemConcourse"))
})


context("Typsetting items")

test_that(desc = "conversion from pdf to svg works", code = {
  skip_on_os(os = c("windows", "mac"))  # no easy way to get pdf2svg
  pdf_input <- "test1.pdf"
  checkmate::expect_file_exists(x = pdf_input)
  pdf2svg(pdf_input = pdf_input)
  checkmate::expect_file_exists(x = "test1.svg")
})

test_that(desc = "pdf card is produced from string", code = {
  skip_on_appveyor()  # does not have latex
  skip_on_os(os = c("mac"))
  output <- pensieve:::make_cards(item_text = "foo", item_handle = "foo_handle")
  checkmate::expect_file_exists(x = output$paths$pdf)
})
