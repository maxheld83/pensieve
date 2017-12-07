#' @title Print pensieve S3 object in knitr documents nicely
#'
#' @section Printing in knitr chunks:
#' Extends the [knitr::knit_print()] generic for pensieve S3 object.
#' When [DT::datatable()] is installed (recommended), and `use_js` evaluates to `TRUE`, an interactive table will be printed.
#' When printr is installed (recommended), a static table depending on the output format will be printed.
#' Otherwise, the object is simply [base::print()]ed.
#'
#' By default `print()` in knitr will default to [knitr::knit_print()], so to nicely print some object `obj` inside a chunk, you can just write `print(obj)` or even just `obj`.
#'
#' However, to manually invoke or preview the interactive displays in RStudio, you must call [knitr::knit_print()] in full.
#' The [base::print()]ing method of the underlying classes is *not* altered outside of a knitr chunk.
#'
#' @param use_js
#' A logical flag, indicating whether an interactive, java-script variant of the printed result should be returned.
#' Applicable only to HTML-outputs.
#' Defaults to `NULL`, in which case the appropriate output is inferred from the runtime environment.
#'
#' @inheritParams knitr::knit_print
#'
#' @family print functions
