test_that("hdtableType", {

  hdtableType <- hdtableType("Num-Cat")

  expect_true(inherits(c(hdtableType("Num"), "Cat"),"hdtableType"))

  y <- hdtableType("Cat")
  expect_true(hdtableType("Cat") == y)

  x <- c("Cat-Num-Cat")
  fr <- hdtableType(x)
  expect_equal(get_hdtableTypeGroup(fr), "Cat2-Num")
  expect_equal(hdtableType_hdtypes(fr), hdtype(c("Cat", "Num", "Cat")))

  x <- c("Cat-Num", "Cat-Num-Cat", "Cat")
  fr <- hdtableType(x)
  expect_equal(get_hdtableTypeGroup(fr), c("Cat-Num","Cat2-Num", "Cat"))
  # With a vector of hdtableTypes, hdtypes become a list
  expect_equal(hdtableType_hdtypes(fr),
               list(
                 hdtype(c("Cat", "Num")),
                 hdtype(c("Cat", "Num", "Cat")),
                 hdtype("Cat")
               ))
  f1 <- fr[1]
  get_hdtableTypeGroup(f1)
  get_hdtableTypeGroup(f1)


})
