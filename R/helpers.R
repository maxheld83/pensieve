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


# helper function to quickly build book ====
# helpful because this is a package, so it's not easy to build the book
render_site2 <- function(book = TRUE, html_only = NULL, landing = TRUE, docs = TRUE, serve = NULL) {
  # infer good defaults
  if (is.null(html_only)) {
    html_only <- is_rstudio()
  }
  if (is.null(serve)) {
    serve <- is_rstudio()
  }

  if (book) {
    withr::with_dir(new = "docs/book/", code = {# this makes changing wd safe
      if (html_only) {
        bookdown::render_book(input = "docs/book/index.Rmd",
                              output_format = 'bookdown::gitbook',
                              output_dir = '../../_site/book',
                              clean_envir = TRUE,
                              envir = new.env())
      } else {
        bookdown::render_book(input = 'index.Rmd',
                              output_format = 'all',
                              output_dir = '../../_site/book',
                              clean_envir = TRUE,
                              envir = new.env())
      }
    })
  }

  if (landing) {
    withr::with_dir(new = "docs/landing/", code = {
      blogdown::build_site()
    })
  }

  if (docs) {
    pkgdown::build_site(path = "_site/docs")
  }

  if (serve) {
    servr::httw(dir = "_site/docs/", daemon = TRUE)
  }
}


# find path to *built* accio, always root of pensieve
# this gives "" if folder does not exist
accio_path <- system.file('accio', package = 'pensieve')

#' @title Run accio
#'
#' @description
#' Run accio, the web frontend for pensieve, if available.
#'
#' @details
#' Accio is the closed-source web frontend for pensieve.
#' Learn more at https://pensieve.maxheld.de.
#'
#' @export
run_accio <- function() {
  # input validation ====
  if (!checkmate::test_directory_exists(accio_path)) {
    stop("The 'accio' web frontend (closed source) is not available on this computer. ",
         "See pensieve.maxheld.de for details.")
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Shiny is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # run it! ====
  shiny::runApp(appDir = accio_path)
}
