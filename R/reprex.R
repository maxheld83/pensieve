f <- function(foo = TRUE, bar = NULL) {
  if (foo) {
    if (is.null(bar)) {
      if (1 + 1 == 2) {
        bar <- TRUE
      }
    }
    if (bar) {
      cat("hello world")
    }
  }
}
