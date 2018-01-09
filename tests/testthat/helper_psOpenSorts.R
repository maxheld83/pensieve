# Creation ====
# you can combine individual sorts into a list ====
los <- psOpenSorts(open_sorts = list(lisa = lisa, peter = peter, rebecca = rebecca))

# or create psOpenSorts from a more convenient input ====
# recreate messy format from canonical form (don't do this at home)
ass <- pensieve:::make_messy(open_sorts = los)$ass
desc <- pensieve:::make_messy(open_sorts = los)$desc
# these two can be conveniently entered in a spreadsheet program
ass
desc

los_from_messy <- import_psOpenSorts(
  assignments_messy = ass,
  descriptions_messy = desc,
  keep_LETTERS = FALSE)
