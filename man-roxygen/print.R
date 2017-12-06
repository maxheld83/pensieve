#' @title Print pensieve S3 object
#' @description Extends the [knitr::knit_print()] generic for pensieve S3 object.
#' Can also be invoked in an interactive session for interactive displays in RStudio.
#' When [DT::datatable()] is installed (recommended), and `use_js` evaluates to `TRUE`, an interactive table will be printed.
#' When [printr][printr] is installed (recommended), a static table depending on the output format will be printed.
#' Otherwise, the object is simply [base::print()]ed.
#'
#' @param ...
#' Arguments passed onto other methods.
#' Not currently used.
#'
#' @param use_js
#' A logical flag, indicating whether an interactive, java-script variant of the printed result should be returned.
#' Applicable only to HTML-outputs.
#' Defaults to `NULL`, in which case the appropriate output is inferred from the runtime environment.
#'
#' @inheritParams knitr::knit_print
#'
#' @family print functions
