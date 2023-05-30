test_that("hdtable", {

  d <- NULL
  f <- hdtableClass$new(d)
  expect_null(f$data)

  create_dic(d)

  d <- cars
  names(d) <- c("Speed", "Dist")

  t1 <- hdtableClass$new(d)

  t1$dic
  t1$name
  expect_true(inherits(t1, "hdtable"))

  expect_true("rcd___id" %in% names(t1$dd))
  expect_true(! "rcd___id" %in% names(t1$data))
  expect_equal(names(t1$df()), c("Speed", "Dist"))
  t1$df_slug()
  expect_true("rcd___id" %in% names(t1$df_slug_rcd()))
  expect_equal(t1$magnitude, 2)
  t1$dic_no_fld()
  t1$dic_csv()





  t2 <- hdtable(d)
  t2$name
  expect_equal(t1$data,t2$data)
  expect_equal(t1$dic |> dplyr::select(-fld___id),
               t2$dic |> dplyr::select(-fld___id))
  expect_equal(t1$name, t2$name)
  expect_equal(t1$df(), t2$df())
  expect_equal(hdtibble_as_basetype(t1$dd) |> dplyr::select(-rcd___id),
               hdtibble_as_basetype(t2$dd) |> dplyr::select(-rcd___id))


  #



})


test_that("Read d correctly with rcd___id",{


  # d <- data.frame(
  #   a = Cat(c("black", "white")),
  #   b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
  #   c = Yea(2001:2002),
  #   d = Num(runif(2)*10),
  #   e = Pct(runif(2))
  # )
  # hdtable(d)
  #
  #
  # d <- jsonlite::read_json(system.file("iris.json", package = "hdtable"),
  #                          simplifyVector = TRUE)



})


