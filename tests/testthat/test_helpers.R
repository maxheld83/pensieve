context("helpers")

test_that(desc = "we create proper names", code = {
  expect_equivalent(
    object = make_unique_names_from_strings(strings = c("foo§$%&/(bar", "zap_zong", "zap zonk", "wop pap", "bop", "wop wap", "Wop", "lis")),
    expected = c("foo", "zap_zong", "zap", "pap", "bop",  "wap",  "sta7", "lis")
  )
  expect_names(
    x = make_unique_names_from_strings(strings = civicon_2014$QItems$concourse[,"german"]),
    type = "strict"
  )
})
