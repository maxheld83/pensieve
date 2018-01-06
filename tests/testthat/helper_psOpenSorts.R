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
# providing no description is possible, but makes interpretation hard, if not meaningless.

# now let's combine the individual sort into a list
open_sorts <- psOpenSorts(open_sorts = list(lisa = lisa, peter = peter, rebecca = rebecca))
