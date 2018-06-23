# example grid to test print functions
grid <- make_grid(x_range = c(-7, 7), y_range = c(1,7), pattern = "honeycomb", offset = "odd")
grid[1:6, c(1:2, 14:15)] <- FALSE
grid[1:5, c(3, 13)] <- FALSE
grid[1:3, c(4, 12)] <- FALSE
grid[1:2, c(5, 11)] <- FALSE
grid[1, c(6, 10)] <- FALSE
grid
