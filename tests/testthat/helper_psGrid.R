# create simple grids ====

# make simple matrix by hand
m <- matrix(data = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE), nrow = 2)
grid_byhand <- psGrid(grid = m)

# matrix with better dimnames
dimnames(m) <- list(
  c(NULL), # rows, or y-dimension is meaningless, used for ties
  desirable = NULL  # no use in adding actual column names
  # say, desirable is the short form for the sorting conditition used on x
)
grid_byhand <- psGrid(grid = m)

# coerce grid from conventional distribution notation
grid_bycoercion <- as_psGrid(obj = c(1,2,1))
