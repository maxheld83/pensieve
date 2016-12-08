# script to build random names
# this is just extract from randomNames package, saved here again to cut down on dependencies
library(randomNames)
# we're only taking last names, because these carry no gender information
# we're not selecting on ethnicity, because that might cause biases and also just make this whole thing harder
# there seems to be no easy way to do this because the whole thing is only an environment
all_names <- c(randomNames::randomNamesData$last_names_e1,
               randomNames::randomNamesData$last_names_e2,
               randomNames::randomNamesData$last_names_e3,
               randomNames::randomNamesData$last_names_e4,
               randomNames::randomNamesData$last_names_e5)
unique_names <- unique(names(all_names))  # there are actually some ethnic duplicates in here

library(checkmate)
strict_names <- sapply(X = unique_names, FUN = function(x) {
  test_names(x = x, type = "strict")
})
good_names <- all_names[unique_names[strict_names]]

library(devtools)
devtools::use_data(good_names, internal = TRUE, overwrite = TRUE)
