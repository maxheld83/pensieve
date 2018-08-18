# helper ====
#' @title Store a *several* sorts in a [tibble::tbl_df].
#' @name psSorts
#' @description
#' Stores *several* sorts of class [psSort] as a list column inside a [tibble::tbl_df], along with arbitrary additional columns.
#' Prepends [psSorts] class.
#'
#' @details
#' It's sometimes helpful to store several sorts in one place, along with additional information, such as time stamps or treatment conditions.
#' You can also use this class to store using slightly different designs, such as different sorting distributions.
#' These different designs can then be coerced to the canonical (matrix) forms in `psClosedSort`.
#'
#' @section Tidy data:
#' [psSorts] is [**tidy data**](http://r4ds.had.co.nz/tidy-data.html):
#' - Each arbitrary variable, as well as the sort has its own column.
#'   The item ranks are *not* saved as individual item-columns, because:
#'   - Item ranks are **ipsative measurements**, and as such are meaningful, if kept and analysed *together*.
#'     Storing them as separate item columns might obscure the ipsative quality of the measurement.
#'   - With item-columns, there would be no easy to way to save **multiple sorting dimensions**.
#'     Stored as matrices in a list column, multiple sorting dimensions can be stored both *in the same sort*, as well as across several rows.
#' - Each observation has its own row.
#'   An observation is a record of **operant subjectivity** at some given point in time, under some conditions.
#'   When using time series, observations can also include *incomplete* sorts, as they are being assembled over time.
#' - Each value has its own cell, including a **snapshot** of a sort in a list column.
#'
#' Tidy data is more useful for storing and importing sorting data, than for analysing it.
#' To analyse sorting data, convert it to the untidy, but conventional matrix form using `as_psClosedSorts`.
#' The matrix form is better suited for analysis, because rows and columns are easily transposable for ipsative sorts and linear algebra operations are meaningful.
#'
#' @section Validation:
#' Use [validate_psSorts()] to validate a group of individual [psSort]s:
#'
#' - If you provide `items`, all sorts must include only a subset of items.
#' - If you provide `grid`, all sorts must conform to a grid.
#'   Notice that this may not be methodologically necessary; as long as the measurement was ipsative, different sorting grids may be meaningfully coerced to one another.
#' @noRd
NULL
