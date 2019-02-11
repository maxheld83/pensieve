#' @title Make unique, syntactically valid names from strings
#' @description Helper for making unique names out of longish strings.
#' @param strings `[character()]` giving some set of strings.
#' @return `[character()]` giving the unique names.
#' @noRd
make_unique_names_from_strings <- function(strings) {
  res <- find_unique_substring(strings)
  stringr::str_replace_all(
    string = res,
    pattern = '([[:punct:]])|\\s+',
    replacement = '_'
  )
}
find_unique_substring <- function(strings) {
  # remove punctuation
  strings <- stringr::str_replace_all(
    string = strings,
    pattern = "[^[:alnum:]_]",
    replacement = " "
  )
  # convert 2 all lower case
  strings <- stringr::str_to_lower(strings)
  # trim whitespace
  strings <- stringr::str_trim(string = strings, side = "both")
  strings <- stringr::str_squish(string = strings)

  # split up into a matrix
  m <- stringr::str_split_fixed(string = strings, pattern = " ", n = Inf)

  for (i in 1:nrow(m)) {
    this_row <- m[i, ]

    # make sure cells are strictly valid R names
    this_row <- stringr::str_subset(string = this_row, pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$")
    # regex is from https://mllg.github.io/checkmate/reference/checkNames.html

    unique_words <- !(this_row %in% m[-i, ])
    if (any(unique_words)) {
      # take words which are globally unique
      strings[i] <- this_row[unique_words][1]
    } else {
      # otherwise, make up a name
      strings[i] <- glue("sta{i}")
    }
  }
  strings
}

is_even <- function(x) x %% 2 == 0

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
is_knitr <- function() {
  isTRUE(getOption('knitr.in.progress'))
}


# assert and infer helpers ====

assert_n_infer_use_js <- function(use_js = NULL) {
  assert_flag(x = use_js,
                         na.ok = FALSE,
                         null.ok = TRUE)
  if (is.null(use_js)) {
    use_js <- is_use_js()
  }
  return(use_js)
}

assert_n_infer_summarize <- function(summarize, x) {
  assert_flag(x = summarize,
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


# system dependency helpers ====
#' @title find root path of current *built* pensieve, and append paths from there.
#' @details
#' Remember to never call this from the top NAMESPACE (outside of functions); that would hardcode an absolute path into the package which might not work on other machines.
#' Notice further that [devtools::system.file()] automatically takes care of this working in a development environment, when [devtools::load_all()] is used, and dependencies are still inside `inst/`.
#' When compiled, these move to the toplevel of the package.
#' @param ... path to a system dependency, relative from `inst/`.
#' @return Absolute path to file.
#' @noRd
pensieve_system_file <- function(...) {
  requireNamespace2("fs")
  file <- fs::path(...)
  system.file(file, package = "pensieve")
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
#' Learn more at https://www.maxheld.de/pensieve
#'
#' @export
run_accio <- function() {
  # input validation ====
  if (!test_directory_exists(accio_path)) {
    stop("The 'accio' web frontend (closed source) is not available on this computer. ",
         "See www.maxheld.de/pensieve for details.")
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

#' @title Error out if no coercion method can be found.
#' @param obj Arbitrary object to be coerced.
#' @param target_class A character vector, target class to be coerced to.
#' @noRd
stop_coercion <- function(obj, target_class) {
  stop(
    glue("Sorry, don't know how to coerce object of class {obj} into a {target_class}."),
   call. = FALSE
  )
}


# documentation ====
#' @title Generate roxygen2 tag for string choice arguments.
#' @description
#' Some arguments will accept only a string from a choice of arguments.
#' This function generates the necessary roxygen2 tag, using an R object to list all the available options.
#' @param arg_name `[character(1)]` giving the argument name
#' @param choices `[character(1)]` giving the available choices
#' @param before,after `[character(1)]` character string before or after the list
#' @param null `[character(1)]` description of what happens the choice is `NULL`.
#' Defaults to `NULL`, in which case `NULL` is not allowed as an argument.
#' @param default `[character(1)]` from `choices` giving the default value, defaults to `NULL` for no default.
#' Use `"null"` for `NULL` as the default choice.
#' @noRd
document_choice_arg <- function(arg_name, choices, before = "", after = "", null = NULL, default = NULL) {
  assert_string(x = arg_name, null.ok = FALSE, na.ok = FALSE)
  assert_character(x = choices, any.missing = FALSE, unique = TRUE, null.ok = FALSE)
  assert_string(x = before, null.ok = FALSE, na.ok = FALSE)
  assert_string(x = after, null.ok = FALSE, na.ok = FALSE)
  assert_string(x = null, na.ok = FALSE, null.ok = TRUE)
  assert_choice(x = default, choices = c(choices, "null"), null.ok = TRUE)

  if (test_named(choices)) {
    choices_list <- glue("- `'{choices}'` for *{names(choices)}*")
  } else {
    choices_list <- glue("- `'{choices}'`")
  }

  if (!is.null(null)) {
    choices_list <- c(as_glue("- `NULL` {null}"), choices_list)
  }

  if (!is.null(default)) {
    if (default == "null") {
      # null choice is always the first
      default_choice_ind <- 1
    } else {
      default_choice_ind <- which(choices == default)
    }
    # append default markup
    choices_list[default_choice_ind] <- glue_collapse(x = c(choices_list[default_choice_ind], "(**default**)"), sep = " ")
  }

  glue(
    "@param {arg_name} `[character(1)]` {before} \n",
    "Must be one of:",
    glue_collapse(choices_list, sep = "\n", last = " or \n"),
    "\n {after}",
    .sep = "\n"
  )
}

