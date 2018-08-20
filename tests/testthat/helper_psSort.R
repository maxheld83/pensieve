one_sort <- matrix(
  data = c(NA, "live_2_work", NA, "work_2_live", NA, NA),
  nrow = 2,
  dimnames = list(
    c(NULL),
    # this is for rownames, of which there are none, because those are just ties
    desirable = NULL  # no really useful dimnames
    # 'desirable' is a short name for the description of the sorting axis
    # (here, as typically, x)
  )
)
one_sort <- psSort(sort = one_sort)

# you can coerce an empty (all `NA`) sort from grid
as_psSort(obj = grid_bycoercion)

# you can coerce a sort from an integer(ish) vector, with cells filled from the bottom up
one_sort_from_vec <- as_psSort(obj = c(foo = -1, bar = 0, zap = 1, zong = 1))
# you can also pass on other arguments to `psSort()`
one_sort_from_vec_hex <- as_psSort(
  obj = c(foo = -1, bar = 0, zap = 1, zong = 1),
  polygon = "hexagon",
  offset = "odd"
)

# you can also coerce a sort from a long data.frame, like so:
df <- tibble::tribble(
  ~x, ~y, ~cell,
  1,   1, "foo",
  # notice that there is no item at x = 2;
  # the missing NA will be added by the below coercion method
  3,   1, "bar"
)
one_sort_from_df <- suppressMessages(as_psSort(df))
# message would inform about no item at position 2
