# create sorts ====
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

# two conditions of instruction are planes in a 3-d array
csorts_multiple_conds <- array(
  data = c(-1, 0, 1, -1, 0, 1, -1, 0),
  dim = c(2, 2, 2),
  dimnames = list(
    people = c("Lisa", "Peter"),
    items = c("live_2_work", "work_2_live"),
    conditions = c("desirable", "probable")
  )
)
csorts_multiple_conds <- psClosedSorts(csorts_multiple_conds)
