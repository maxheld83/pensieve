# capitalize first letter of word in string ====
# useful for making S3 names
# from http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
capitalize <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)),
        substring(s, 2),
        sep = "",
        collapse = " ")
}

# helper to append, and only if class not already in there
# this is to avoid duplicate classes, which is just confusing
classify_clever <- function(x, classname) {
  if (!inherits(x = x, what = classname)) {
    class(x) <- append(classname, class(x))
  }
  return(x)
}

# helper function to deal with validation stuff in class construction ====
assert_class2 <- function(x, validate) {
  assert_flag(x = validate,
              na.ok = FALSE,
              null.ok = FALSE)
  if (validate) {
    assert(x)
  }
}


# detect whether runtime is suitable for interactive stuff (HTML or RStudio) ====
is_rstudio <- function() {
  isTRUE(interactive() & Sys.getenv("RSTUDIO") == 1)
}
is_html <- function() {
  isTRUE(knitr::opts_knit$get("rmarkdown.pandoc.to") == "html")
}
is_use_js <- function() {
  isTRUE(is_rstudio() | is_html())
}


# assert and infer helpers ====

assert_n_infer_use_js <- function(use_js) {
  checkmate::assert_flag(x = use_js,
                         na.ok = FALSE,
                         null.ok = TRUE)
  if (is.null(use_js)) {
    use_js <- is_use_js()
  }
  return(use_js)
}

assert_n_infer_summarize <- function(summarize, x) {
  checkmate::assert_flag(x = summarize,
                         na.ok = FALSE,
                         null.ok = TRUE)
  if (is.null(summarize)) {
    # notice that this could be made into a generic with s3 methods, then we could have specific cutoffs for each kind of object. Just sayin.
    p_number <- nrow(x)
    if (p_number <= 50) {
      summarize <- FALSE
    } else {
      summarize <- TRUE
    }
  }
  return(summarize)
}



# import frequently used packages ====
# this is so we don't need :: whenever calling one of those
#' @import ggplot2
#' @import checkmate
#' @import testthat
NULL


# helper function to quickly build book ====
# helpful because this is a package, so it's not easy to build the book
render_site2 <- function(wd = "book/", output_format = "bookdown::gitbook", ...) {
  setwd(wd)
  if (is_rstudio()) {
    rmarkdown::render_site(output_format = output_format)
    rstudioapi::viewer("book/_book/index.html")
  } else {
    bookdown::render_book(input = 'index.Rmd', output_format = 'bookdown::gitbook', output_dir = '../_book')
    bookdown::render_book(input = 'index.Rmd', output_format = 'bookdown::pdf_book', output_dir = '../_book')
    bookdown::render_book(input = 'index.Rmd', output_format = 'bookdown::epub_book', output_dir = '../_book')
  }
  setwd("..")
}


# helper to convert pdf to svg ====
pdf2svg <- function(pdf_input) {
  # Input validation
  pdf_input <- c("test.pdf", "test2.pdf")
  checkmate::assert_file_exists(x = pdf_input, extension = "pdf")
  checkmate::assert_character(x = pdf_input, any.missing = FALSE, unique = TRUE)
  checkmate::assert_path_for_output(x = getwd(), overwrite = TRUE)
  checkmate::assert_os(os = "mac")

  # vectorized!
  for (i in pdf_input) {
    file_i <- tools::file_path_sans_ext(i)
    # assemble command
    cmd <- paste("pdf2svg",
                 i,
                 paste0(file_i, ".svg"),
                 "1",
                 sep = " ")
    system(cmd)
  }



}
