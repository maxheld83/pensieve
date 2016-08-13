# this script was used to create the import example *.csv for the q-cat import functions
# these data are from Verena Kasztantowicz' "komki" study
# the script to create these data from the raw google spreadsheet entries, in turn, is in VK's "komki" repository, though is probably not necessary going forward.
# this data is NOT TO BE CONFUSED with the readily-imported, cleaned and tested komki data, which ships as an example dataset as an RDA with the package!

# master data
write.csv(x = komki$qcat$desc,
          file = "../pensieve/inst/extdata/import_example/q_cat/komki/cat_desc.csv",
          quote = TRUE,
          row.names = TRUE)
desc_test <- read.csv(file = "../pensieve/inst/extdata/import_example/q_cat/komki/cat_desc.csv",
                             header = TRUE,
                             stringsAsFactors = FALSE,
                             row.names = 1)
# this read in procedure should be copied to the example for the import function!
komki$qcat$desc == desc_test  # checks out

# category data
write.csv(x = komki$qcat$ass,
          file = "../pensieve/inst/extdata/import_example/q_cat/komki/cat_ass.csv",
          row.names = TRUE)
ass_test <- read.csv(file = "../pensieve/inst/extdata/import_example/q_cat/komki/cat_ass.csv",
                     header = TRUE,
                     stringsAsFactors = FALSE,
                     row.names = 1)
# this read in procedure should be copied to the example for the import function!
komki$qcat$ass == ass_test  # checks out
