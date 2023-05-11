test_that("dic",{

  d <- mtcars
  new_dic <- create_dic(d)

  expect_equal(new_dic$hdType, hdTableType_hdTypes(guess_hdTableType(d)))

  expect_true(class(new_dic$format) == "list")
  expect_true(class(new_dic$stats) == "list")

  d <- cars
  new_dic <- create_dic(d, hdTableType = "Num-Num")
  expect_equal(new_dic$hdType, hdType(c("Num", "Num")))

  d <- sample_data("Cat-Num")
  new_dic <- create_dic(d)
  expect_equal(firstup(names(d)), new_dic$label)

  d <- tibble::tibble(x = Cat(1:2), y = Num(1:2))
  new_dic <- create_dic(d)
  expect_equal(new_dic$hdType, hdType(c("Cat", "Num")))

  # d <- tibble::tibble(x = "24/06/2020")
  # new_dic <- create_dic(d)
  # expect_equal(new_dic$hdType, hdType("Dat"))

  d <- tibble::tibble("Helloo X" = 1, "x 43" = as.Date("2020-04-21"))
  dic <- tibble::tibble(
    id = c("helloo_x", "x_43"),
    label = c("Helloo X", "x 43"),
    hdType = hdType(c("Num", "Cat"))
    )
  t <- hdTable(d, dic)

  d$`Helloo X` <- 2
  dic_updated <- update_dic(dic, d)

  expect_true(class(dic_updated$format) == "list")
  expect_true(class(dic_updated$stats) == "list")



})
