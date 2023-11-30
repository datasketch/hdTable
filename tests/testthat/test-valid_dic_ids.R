test_that("Valid dic ids", {

  dic <- data.frame(id = c("valid_name", "valid_name_123"))
  expect_true(valid_dic_ids(dic))

  dic <- data.frame(id = c("Invalid_name", "rcd__id"))
  expect_false(valid_dic_ids(dic))

  invalid_dic <- data.frame(id = c("Invalid_name", "rcd__id"))
  expect_false(valid_dic_ids(invalid_dic))

  invalid_dic <- data.frame(id = c("invalid_name_", "rcd__id"))
  expect_false(valid_dic_ids(invalid_dic))

  invalid_dic <- data.frame(id = c("_invalid_name", "rcd__id"))
  expect_false(valid_dic_ids(invalid_dic))

  invalid_dic <- data.frame(id = c("123_invalid_name", "rcd__id"))
  expect_false(valid_dic_ids(invalid_dic))

  ##

  d <- cars
  names(d) <- c("a", "b")

  dic <- data.frame(label = c("speed", "dist"),
                    id = c("a", "b"),
                    description = c("SPEED", "DIST"),
                    hdtype = c("Num", "Num"))

  expect_true(valid_dic_ids(dic, d))


  dic <- data.frame(id = c("helloo_x", "x_43", "cats"))
  valid_dic_ids(dic)

})
