# helper to convert pdf to svg ====
pdf2svg <- function(pdf_input) {
  # Input validation
  checkmate::assert_file_exists(x = pdf_input, extension = "pdf")
  checkmate::assert_character(x = pdf_input, any.missing = FALSE, unique = TRUE)
  checkmate::assert_path_for_output(x = getwd(), overwrite = TRUE)
  checkmate::assert_os(os = c("mac", "linux"))

  # vectorized!
  for (i in pdf_input) {
    file_i <- tools::file_path_sans_ext(i)
    # assemble command
    system2(command = "pdf2svg",
            args = c(i, paste0(file_i, ".svg"), "1"),
            stderr = "")
  }
}
