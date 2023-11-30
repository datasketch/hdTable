test_that("Valid dic ids", {

  dic <- data.frame(id = c("valid_name", "valid_name_123"))
  validate_dic_ids(dic)

  dic <- data.frame(id = c("Invalid_name", "rcd__id"))
  expect_error(validate_dic_ids(dic))

  invalid_dic <- data.frame(id = c("Invalid_name", "rcd__id"))
  expect_error(validate_dic_ids(invalid_dic))

  invalid_dic <- data.frame(id = c("invalid_name_", "rcd__id"))
  expect_error(validate_dic_ids(invalid_dic))

  invalid_dic <- data.frame(id = c("_invalid_name", "rcd__id"))
  expect_error(validate_dic_ids(invalid_dic))

  invalid_dic <- data.frame(id = c("123_invalid_name", "rcd__id"))
  expect_error(validate_dic_ids(invalid_dic))

  ##

  d <- cars
  names(d) <- c("a", "b")

  dic <- data.frame(label = c("speed", "dist"),
                    id = c("a", "b"),
                    description = c("SPEED", "DIST"),
                    hdtype = c("Num", "Num"))

  validate_dic_ids(dic, d)


  dic <- data.frame(id = c("helloo_x", "x_43"))
  d <- data.frame("helloo_x" = 1, "x_43" = 2, "nope" = 3, "other" = 4)
  expect_error(validate_dic_ids(dic, d))

  d <- data.frame("helloo_x" = 1)
  expect_error(validate_dic_ids(dic, d))



})
