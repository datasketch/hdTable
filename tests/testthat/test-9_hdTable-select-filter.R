# test_that("hdtable column extraction works",{
#
#   f <- sample_hdtable("Cat-Dat-Num-Pct", n = 11,
#                       names = c("Category", "Dates", "Numbers","Percentages"))
#   f
#   expect_equal(hdtable_column(f,1),as_baseType(f$data[[1]]))
#   expect_equal(hdtable_column(f,"Dates"), as_baseType(f$data[[2]]))
#   expect_equal(hdtable_column(f,"c"), hdtable_d(f)[[3]])
#
# })
#
