test_that("hdtable", {

  d <- cars
  t1 <- hdtableClass$new(d)
  t1$name
  expect_true(inherits(t1, "hdtable"))

  t2 <- hdtable(d)
  t2$name
  expect_equal(t1$data,t2$data)
  expect_equal(t1$dic, t2$dic)
  expect_equal(t1, t2)

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
