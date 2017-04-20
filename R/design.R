make_grid <- function(x_range = c(-5,5), y_range = c(1,5), pattern = "chessboard") {
  # Initialisation (for testing only) ====
  if (FALSE) {
    x_range <- c(-5,5)
    y_range <- c(1,5)
    pattern = "chessboard"
  }

  # Input validation ====
  assert_vector(x = x_range,
                strict = FALSE,
                any.missing = FALSE,
                all.missing = FALSE,
                len = 2,
                unique = TRUE,
                null.ok = FALSE)
  assert_vector(x = y_range,
                strict = FALSE,
                any.missing = FALSE,
                all.missing = FALSE,
                len = 2,
                unique = TRUE,
                null.ok = FALSE)
  assert_choice(x = pattern,
                choices = c("chessboard"))

  # Data preparation ====
  x_values <- c(min(x_range):max(x_range))
  y_values <- c(min(y_range):max(y_range))

  if (pattern == "chessboard") {
    m <- matrix(data = TRUE, nrow = length(y_values), ncol = length(x_values), dimnames = list(y = as.character(y_values), x = as.character(x_values)))
  }
  return(m)
}
