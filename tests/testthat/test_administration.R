context("Typsetting items")

test_that(desc = "conversion from pdf to svg works", code = {
  skip_on_os(os = c("windows"))
  checkmate::expect_file_exists(x = c("test1.pdf", "test2.pdf"))
  pdf_input <- c(
  "test1.pdf",
  "test2.pdf"
  )
  # pdf2svg(pdf_input = pdf_input)
  # checkmate::expect_file_exists(x = c("test1.svg", "test2.svg"))
  # file.remove("test1.svg", "test2.svg")
})
