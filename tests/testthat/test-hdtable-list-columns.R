test_that("hdtable with list columns", {

  library(tibble)
  d <- tibble(
    a = 1:6 * 10,
    b = list(1:3, 2, 5:7, double(0), 5, NA)
  )
  create_dic(d)

  t <- hdtable(d)

  expect_true(is_Nums(t$dd$b))

  library(tibble)
  d <- tibble(
    a = 1:4 * 10,
    b = list(letters[1:3], "b", letters[5:7], character(0))
  )
  t <- hdtable(d)
  expect_true(is_Cats(t$dd$b))

  d <- tibble(
    a = rep(list("Unique"), 500)
  )
  t <- hdtable(d)


})



