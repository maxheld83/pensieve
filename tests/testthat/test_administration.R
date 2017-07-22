context("Typsetting items")

test_that(desc = "conversion from pdf to svg works", code = {
  skip_on_os(os = c("windows"))  # no easy way to get pdf2svg
  checkmate::expect_file_exists(x = c("test1.pdf", "test2.pdf"))
  pdf_input <- c(
  "test1.pdf",
  "test2.pdf"
  )
  pdf2svg(pdf_input = pdf_input)
  checkmate::expect_file_exists(x = c("test1.svg", "test2.svg"))
})

test_that(desc = "pdf card is produced from string", code = {
  skip_on_appveyor()  # does not have latex
  make_cards(text = "foo")
  checkmate::expect_file_exists(x = "simple_rect.pdf")
})
