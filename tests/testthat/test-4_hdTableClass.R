test_that("hdTable", {

  d <- cars
  t1 <- hdTableClass$new(d)

  expect_true(inherits(t1, "hdTable"))

  t2 <- hdTable(d)
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
  hdTable(d)



})
