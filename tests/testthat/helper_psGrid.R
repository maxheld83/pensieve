# make simple matrix by hand
m <- matrix(data = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE), nrow = 2)
grid_byhand <- psGrid(grid = m)

# matrix with better dimnames
dimnames(m) <- list(
  c(NULL), # rows, or y-dimension is meaningless, used for ties
  desirable = as.character(x = -1:1)
  # say, desirable is the short form for the sorting conditition used on x
)
grid_byhand <- psGrid(grid = m)

# coerce grid from conventional distribution notation
grid_bycoercion <- as_psGrid(obj = c(1,2,1))
