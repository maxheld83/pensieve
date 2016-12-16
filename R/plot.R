# import frequently used packages ====
# this is so we don't need :: whenever calling one of those
#' @import ggplot2
NULL

# detect whether runtime is suitable for interactive plot ====
is_iplot <- function() {
  if (isTRUE(interactive() & Sys.getenv("RSTUDIO") == 1)) {
    iplot <- TRUE
  } else if (isTRUE(knitr::opts_knit$get("rmarkdown.pandoc.to") == "html")) {
    iplot <- TRUE
  } else {
    iplot <- FALSE
  }
  return(iplot)
}

