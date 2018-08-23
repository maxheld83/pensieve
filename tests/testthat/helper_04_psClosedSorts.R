# create multiple sorts ====
# simple case with one condition of instruction
csorts <- matrix(
  data = c(-1, 0, 1, -1),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    people = c("Lisa", "Peter"),
    items = c("live_2_work", "work_2_live")
  )
)
csorts <- psClosedSorts(csorts)

# you can coerce a *row* (person) of a 'psClosedSorts' object from a *single* "raw" psSort
csorts_from_one_sort <- as_psClosedSorts(obj = one_sort)
# this also works for psSort with offsets
csorts_from_one_sort_hex <- as_psClosedSorts(obj = one_sort_from_vec_hex)
