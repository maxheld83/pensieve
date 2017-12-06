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

assert_n_infer_use_js <- function(use_js = NULL) {
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

  requireNamespace2(x = "shiny")

  # run it! ====
  shiny::runApp(appDir = accio_path)
}

# always returns TRUE or FALSE (if error = FALSE) or error
requireNamespace2 <- function(x, error = TRUE, msg = NULL) {
  if (!requireNamespace(x, quietly = TRUE)) {
    if (error) {
      if (is.null(msg)) {
        stop(
          paste(
            x,
            "package is needed for this function to work.",
            "Please install it."),
          call. = FALSE)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      if (is.null(msg)) {
        warning(
          paste(
            "Function works better with package",
            x,
            "installed.",
            "Please install it."),
          call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
      }
    }
    return(FALSE)
  } else {
    return(TRUE)
  }
}
