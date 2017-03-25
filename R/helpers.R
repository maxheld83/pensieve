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


# import frequently used packages ====
# this is so we don't need :: whenever calling one of those
#' @import ggplot2
NULL


# helper function to quickly build book ====
# helpful because this is a package, so it's not easy to build the book
render_site2 <- function(wd = "book/", output_format = "bookdown::gitbook", ...) {
  setwd(wd)
  rmarkdown::render_site(output_format = output_format)
  setwd("..")
  if (is_rstudio()) {
    rstudioapi::viewer("book/_book/index.html")
  }
}
