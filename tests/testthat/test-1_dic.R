test_that("dic",{

  d <- mtcars
  new_dic <- create_dic(d)

  expect_equal(new_dic$hdtype, hdtableType_hdtypes(guess_hdtableType(d)))

  expect_true(class(new_dic$format) == "list")
  expect_true(class(new_dic$stats) == "list")

  d <- cars
  new_dic <- create_dic(d, hdtableType = "Num-Num")
  expect_equal(new_dic$hdtype, hdtype(c("Num", "Num")))

  d <- sample_data("Cat-Num")
  new_dic <- create_dic(d)
  expect_equal(firstup(names(d)), new_dic$label)

  d <- tibble::tibble(x = Cat(1:2), y = Num(1:2))
  new_dic <- create_dic(d)
  expect_equal(new_dic$hdtype, hdtype(c("Cat", "Num")))

  # d <- tibble::tibble(x = "24/06/2020")
  # new_dic <- create_dic(d)
  # expect_equal(new_dic$hdtype, hdtype("Dat"))

  d <- tibble::tibble("Helloo X" = 1, "x 43" = as.Date("2020-04-21"))
  dic <- tibble::tibble(
    id = c("helloo_x", "x_43"),
    label = c("Helloo X", "x 43"),
    hdtype = hdtype(c("Num", "Cat"))
    )
  t <- hdtable(d, dic)

  d$`Helloo X` <- 2
  dic_updated <- update_dic(dic, d)

  expect_true(class(dic_updated$format) == "list")
  expect_true(class(dic_updated$stats) == "list")



})
