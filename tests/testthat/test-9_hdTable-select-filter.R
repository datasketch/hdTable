# test_that("hdTable column extraction works",{
#
#   f <- sample_hdTable("Cat-Dat-Num-Pct", n = 11,
#                       names = c("Category", "Dates", "Numbers","Percentages"))
#   f
#   expect_equal(hdTable_column(f,1),as_baseType(f$data[[1]]))
#   expect_equal(hdTable_column(f,"Dates"), as_baseType(f$data[[2]]))
#   expect_equal(hdTable_column(f,"c"), hdTable_d(f)[[3]])
#
# })
#
