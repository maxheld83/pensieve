# create single open sort ====

# Lisas open sort, matching by index
assignments <- matrix(data = c(TRUE, FALSE, FALSE, TRUE),
                      nrow = 2,
                      dimnames = list(items = c("cat", "dog")))
descriptions <- c("a pet which largely takes care of itself",
                  NA)
lisa <- psOpenSort(assignments = assignments, descriptions = descriptions)

# Peters open sort, matching by name
assignments <- matrix(data = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
                      nrow = 2,
                      dimnames = list(items = c("cat", "dog"),
                                      categories = c("in_homes",
                                                     "quiet",
                                                     "herbivore")))
descriptions <- c(in_homes = "Animal found in peoples homes.",
                  quiet = "Does not make a lot of noise.",
                  herbivore = "Eats plants.")
peter <- psOpenSort(assignments = assignments, descriptions = descriptions)

# Rebeccas open sort, without any descriptions provided
assignments <- matrix(data = c(FALSE, FALSE, TRUE, TRUE),
                      nrow = 2,
                      dimnames = list(handles = c("cat", "dog")))
rebecca <- psOpenSort(assignments = assignments, descriptions = NULL)


# now let's combine the individual sorts into a list ====
open_sorts <- psOpenSorts(open_sorts = list(lisa = lisa, peter = peter, rebecca = rebecca))


# create psOpenSorts from convenient input ====
# this is just to wrangle the above canonical data into the messy, but convenient form
# users never have to do this part; it's just for illustration
# instead, users can input the result in a spreadsheet program
ass <- sapply(X = open_sorts, FUN = function(part) {
  apply(X = part, MARGIN = 1, FUN = function(x) {
    paste(LETTERS[1:length(x)][x], collapse = ", ")
  })
})
ass  # this is easy to enter in a spreadsheet program

maxlength <- 3
l <- sapply(X = open_sorts, simplify = TRUE, FUN = function(x) {
  vec <- unlist(attr(x = x, which = "descriptions"))
  length(vec) <- maxlength
  return(vec)
})
desc <- do.call(what = cbind, args = l)
rownames(desc) <- LETTERS[1:nrow(desc)]
desc <- cbind(desc, NA)
colnames(desc) <- names(open_sorts)
desc  # this is easy to enter in a spreadsheet program

open_sorts_from_messy <- import_psOpenSorts(assignments_messy = ass, descriptions_messy = desc)
