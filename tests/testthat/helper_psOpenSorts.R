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

ass <- matrix(data = c("A, B",
                       # meaning A and B are assigned
                       "",
                       # meaning no category assigned
                       "B",
                       # only B assigned
                       NA),
                       # item never considered for assignment across *all* categories or vice versa
                       nrow = 2,
                       ncol = 2,
                       dimnames = list(items = c("cat", "dog"),
                       people = c("tony", "amy")))
desc <- matrix(data = c("",
                        # will be treated as NA
                        NA,
                        # participant provided no description, but assigned the category
                        "lives in cage",
                        # described, but never assigned
                        NA,
                        # never assigned, never described will be removed
                        "actually a predator!",
                        "lives on a farm"
                        # described, but never assigned
                        ),
                        nrow = 3,
                        dimnames = list(categories = c("A", "B", "C"),
                        people = c("tony", "amy")))
# notice how individual *nominal* categories are pasted together in cells here;
# this convenient form *only* works for logical data
osorts_example <- import_psOpenSorts(assignments_messy = ass, descriptions_messy = desc)
