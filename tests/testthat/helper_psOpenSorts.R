# create single open sort ====

# Lisas open sort, unnamed descriptions (matched by index)
assignments <- matrix(
  data = c(TRUE, FALSE, FALSE, TRUE),
  nrow = 2,
  dimnames = list(items = c("cat", "dog")))
descriptions <- c(
  "a pet which largely takes care of itself",
  NA  # dimension is assigned, but not described (not a problem)
)
lisa <- psOpenSort(assignments = assignments, descriptions = descriptions)

# Peters open sort, named descriptions (*also* matched by index)
assignments <- matrix(
  data = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  nrow = 2,
  dimnames = list(
    items = c("cat", "dog"),
    categories = c("in_homes", "quiet", "herbivore")
))
descriptions <- c(
  in_homes = "Animal found in peoples homes.",
  quiet = "Does not make a lot of noise.",
  herbivore = "Eats plants.")  # defined, but never assigned (not a problem)
peter <- psOpenSort(assignments = assignments, descriptions = descriptions)

# Rebeccas open sort, without any descriptions provided
assignments <- matrix(
  data = c(FALSE, FALSE, TRUE, TRUE),
  nrow = 2,
  dimnames = list(handles = c("cat", "dog")))
rebecca <- psOpenSort(assignments = assignments, descriptions = NULL)

# now let's combine the individual sorts into a list ====
los <- psOpenSorts(open_sorts = list(lisa = lisa, peter = peter, rebecca = rebecca))


# create psOpenSorts from convenient input ====
# recreate messy format from canonical form (don't do this at home)
ass <- pensieve:::make_messy(open_sorts = los)$ass
desc <- pensieve:::make_messy(open_sorts = los)$desc
# these two can be conveniently entered in a spreadsheet program
ass
desc

los_from_messy <- import_psOpenSorts(assignments_messy = ass, descriptions_messy = desc)

