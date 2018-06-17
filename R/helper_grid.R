# example grid to test print functions
grid <- make_grid(x_range = c(-5, 5), y_range = c(1,5), pattern = "honeycomb", offset = "odd")
grid[1:4,1] <- FALSE
grid[1:3,2] <- FALSE
grid[1:2,3] <- FALSE
grid[1,4] <- FALSE
grid[1,8] <- FALSE
grid[1:2,9] <- FALSE
grid[1:3, 10] <- FALSE
grid[1:4, 11] <- FALSE
