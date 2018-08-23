# multiple sorts ====
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
