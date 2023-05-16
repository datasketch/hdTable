test_that("hdtable", {

  d <- NULL
  f <- hdtableClass$new(d)
  expect_null(f$data)


  d <- cars
  t1 <- hdtableClass$new(d)
  t1$name
  expect_true(inherits(t1, "hdtable"))

  t2 <- hdtable(d)
  t2$name
  expect_equal(t1$data,t2$data)
  expect_equal(t1$dic |> dplyr::select(-fld___id),
               t2$dic |> dplyr::select(-fld___id))
  expect_equal(t1$name, t2$name)
  expect_equal(t1$d(), t2$d())
  expect_equal(hdtibble_as_basetype(t1$dd) |> dplyr::select(-rcd___id),
               hdtibble_as_basetype(t2$dd) |> dplyr::select(-rcd___id))

  d <- data.frame(
    a = Cat(c("black", "white")),
    b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
    c = Yea(2001:2002),
    d = Num(runif(2)*10),
    e = Pct(runif(2))
  )
  hdtable(d)

  t <- hdtable(iris)
  t$name


})
