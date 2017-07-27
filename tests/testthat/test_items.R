context("Typsetting items")

test_that(desc = "conversion from pdf to svg works", code = {
  skip_on_os(os = c("windows"))  # no easy way to get pdf2svg
  pdf_input <- "test1.pdf"
  checkmate::expect_file_exists(x = pdf_input)
  pdf2svg(pdf_input = pdf_input)
  checkmate::expect_file_exists(x = "test1.svg")
})

test_that(desc = "pdf card is produced from string", code = {
  skip_on_appveyor()  # does not have latex
  output <- pensieve:::make_cards(item_text = "foo", item_handle = "foo_handle")
  checkmate::expect_file_exists(x = output$paths$pdf)
})
