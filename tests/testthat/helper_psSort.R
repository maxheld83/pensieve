sort <- matrix(
  data = c(NA, "live_2_work", NA, "work_2_live", NA, NA),
  nrow = 2,
  dimnames = list(
    c(NULL),
    # this is for rownames, of which there are none, because those are just ties
    desirable = as.character(-1:1)
    # 'desirable' is a short name for the description of the sorting axis
    # (here, as typically, x)
  )
)
psSort(sort = sort)
