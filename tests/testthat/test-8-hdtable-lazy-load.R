test_that("hdtable lazy load", {


  path <- "tmp/files"
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  ## Create and hdtibble from different file types


  # CSV

  # Small CSV
  file_path <- file.path(path, "cars.csv")

  if(!file.exists(file_path)){
    readr::write_csv(cars, file_path)
  }
  d <- file_path
  expect_error(hdtable(d, lazy = TRUE), "If lazy need to provide dictionary")
  dic <- create_dic(vroom::vroom(d, show_col_types = FALSE))
  ht_lazy <- hdtable(d, dic = dic, lazy = TRUE)
  expect_equal(ht_lazy$slug, "cars")

  expect_null(ht_lazy$dd)


  ht_lazy$df()
  expect_true(!is.null(ht_lazy$data))
  expect_equal(names(ht_lazy$dd),c("speed", "dist", "rcd___id"))


  # t <- hdtableClass$new(vroom::vroom(file))

  # Bigger CSV
  file_path <- file.path(path, "nycflights.csv")
  if(!file.exists(file_path)){
    #flights <- nycflights13::flights
    #readr::write_csv(dic, "tests/testthat/tmp/files/nycflights.dic.csv")
    readr::write_csv(nycflights13::flights, file_path)
  }
  if(!file.exists("tmp/files/nycflights.dic.csv")){
    flights <- nycflights13::flights
    dic <- create_dic(flights)
    #readr::write_csv(dic, "tests/testthat/tmp/files/nycflights.dic.csv")
    readr::write_csv(dic, "tmp/files/nycflights.dic.csv")
  }

  dic <- vroom::vroom("tmp/files/nycflights.dic.csv", show_col_types = FALSE)
  t <- hdtable(file_path, dic = dic)

  expect_equal(t$d_path, file_path)
  expect_null(t$dd)

  expect_equal(t$slug, "nycflights")

  expect_equal(nrow(t$data), t$metadata()$nrow)
  expect_true(!is.null(t$dd))

  # Json


  # JSON Stream


  ### hdtable_read
  path <- "tmp/files/table_flights"
  hdtable_write(t, path)

  t2 <- hdtable_read(path)

  expect_null(t2$dd)

  expect_equal(t2$slug, "nycflights")

  expect_equal(nrow(t2$data), t2$metadata()$nrow)
  expect_true(!is.null(t2$dd))

  #unlink("tmp/files", recursive = TRUE)

})
