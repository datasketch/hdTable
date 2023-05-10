test_that("hdTableType", {

  hdTableType <- hdTableType("Num-Cat")

  expect_true(inherits(c(hdTableType("Num"), "Cat"),"hdTableType"))

  y <- hdTableType("Cat")
  expect_true(hdTableType("Cat") == y)

  x <- c("Cat-Num-Cat")
  fr <- hdTableType(x)
  expect_equal(get_hdTableTypeGroup(fr), "Cat2-Num")
  expect_equal(hdTableType_hdTypes(fr), hdType(c("Cat", "Num", "Cat")))

  x <- c("Cat-Num", "Cat-Num-Cat", "Cat")
  fr <- hdTableType(x)
  expect_equal(get_hdTableTypeGroup(fr), c("Cat-Num","Cat2-Num", "Cat"))
  # With a vector of hdTableTypes, hdtypes become a list
  expect_equal(hdTableType_hdTypes(fr),
               list(
                 hdType(c("Cat", "Num")),
                 hdType(c("Cat", "Num", "Cat")),
                 hdType("Cat")
               ))
  f1 <- fr[1]
  get_hdTableTypeGroup(f1)
  get_hdTableTypeGroup(f1)


})
