# the script to build komki_2016ff.rda can be found in Verena Kasztantowicz' komki repository
# notice that the data is *different* from the raw data included as *csv.
# also notice that the study wasn't completed at the time of this writing
load(file = "data/komki_2016ff.rda")
devtools::use_data(komki, overwrite = TRUE)
