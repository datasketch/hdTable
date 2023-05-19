test_that("hdtable funs", {

  nms <- c("Category", "Dates", "Numbers","Percentages")
  f <- sample_hdtable("Cat-Dat-Num-Pct", n = 11,
                     names = nms)
  f

  expect_equal(hdtable_labels(f), nms)

  f$df()
  f$data
  expect_equal(hdtable_column(f,1),f$data[[1]])
  expect_equal(hdtable_column(f,"Dates"), f$data[[2]])
  expect_equal(hdtable_column(f,"numbers"), hdtable_data(f)[[3]])

})
