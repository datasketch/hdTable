test_that("hdtable_type", {

  hdtable_type <- hdtable_type("Num-Cat")

  expect_true(inherits(c(hdtable_type("Num"), "Cat"),"hdtable_type"))

  y <- hdtable_type("Cat")
  expect_true(hdtable_type("Cat") == y)

  x <- c("Cat-Num-Cat")
  fr <- hdtable_type(x)
  expect_equal(get_hdtable_type_group(fr), "Cat2-Num")
  expect_equal(hdtable_type_hdtypes(fr), hdtype(c("Cat", "Num", "Cat")))

  x <- c("Cat-Num", "Cat-Num-Cat", "Cat")
  fr <- hdtable_type(x)
  expect_equal(get_hdtable_type_group(fr), c("Cat-Num","Cat2-Num", "Cat"))
  # With a vector of hdtable_types, hdtypes become a list
  expect_equal(hdtable_type_hdtypes(fr),
               list(
                 hdtype(c("Cat", "Num")),
                 hdtype(c("Cat", "Num", "Cat")),
                 hdtype("Cat")
               ))
  f1 <- fr[1]
  get_hdtable_type_group(f1)
  get_hdtable_type_group(f1)


})
