#' @section Printing in knitr chunks:
#' Extends the [knitr::knit_print()] generic for pensieve S3 objects.
#'
#' By default `print()` in knitr will default to [knitr::knit_print()], so to nicely print some object `obj` inside a chunk, you can just write `print(obj)` or even just `obj`.
#'
#' However, to manually invoke or preview the interactive displays in RStudio, you must call [knitr::knit_print()] in full.
#' The [base::print()]ing method of the underlying classes is *not* altered outside of a knitr chunk.
#'
#' @inheritParams knitr::knit_print
#' @inheritParams knit_print_docs
#'
#' @family print functions
