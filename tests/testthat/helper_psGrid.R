# make simple matrix by hand
m <- matrix(data = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE), nrow = 2)
grid_byhand <- psGrid(grid = m)

# coerce grid from conventional distribution notation
grid_bycoercion <- as_psGrid(obj = c(1,2,1))
