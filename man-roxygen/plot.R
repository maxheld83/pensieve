#' @description Plot S3 class.
#'
#' @param ...
#' Arguments passed onto other methods.
#' Not currently used.
#'
#' @param use_js
#' A logical flag, indicating whether an interactive, java-script variant of the plot should be returned.
#' Defaults to `NULL`, in which case the appropriate output is inferred from the runtime environment.
#'
#' @family plotting functions
#'
#' @templateVar summarize
#' A logical flag, indicating whether the object should be summarized before plotting.
#' If `TRUE`, a plot with a density estimate of individual data points is returned, suitable for medium to large objects (numbers of people-variables).
#' If `FALSE`, a plot with individual data points is returned, suitable for small to medium-sized objects (numbers of people-variables).
#'
#' Defaults to `NULL`, in which case the appropriate plot is inferred from the size.
