test_that("hdTable", {

  d <- hdTable(NULL)
  expect_null(d$data)


  # NA hdTables
  d <- data.frame(x = NA)
  dic <- create_dic(data.frame(x = NA))
  dd <- hdTibble(d, dic = dic)
  expect_equal(UKT(d$x), dd$x)
  #expect_equal(as.logical(dd$x), NA) # Casting to logical not working

  d <- data.frame(x = NA)
  dic <- create_dic(d)
  hdTibble(d, dic)
  hdtab <- hdTable(d)

  hdtab$stats

  f <- hdTable(cars)
  length(f) # 13 ----> fields in the class

  fr <- hdTable(cars)
  expect_equal(f$data, fr$data)
  expect_equal(f$dic, fr$dic)
  # expect_equal(f$frtype, fr$frtype) ### TODO
  expect_equal(f$group, fr$group)

  f2 <- list(hdTable(mtcars), hdTable(cars))
  expect_true(inherits(hdTable(cars),"hdTable"))


  d <- sample_data("Cat-Dat-Num-Pct", n = 11,
                  names = c("Category", "Dates", "Numbers","Percentages"))
  names(d)
  f <- hdTable(d)
  expect_equal(d, f$data)

  names(d) <- dstools::create_slug(names(d))
  expect_equal(d, f$d())

  expect_equal(hdTable_stats(f)$nrow, 11)
  expect_equal(hdTable_stats(f)$ncol, 4)

})

test_that("hdTable creation with dictionaries work",{

  d <- cars
  names(d) <- c("a", "b")
  dic <- data.frame(label = c("speed", "dist"),
                    id = c("speed", "dist"),
                    description = c("SPEED", "DIST"),
                    hdType = c("Num", "Num"))
  f <- hdTable(d, dic = dic)
  expect_equal(hdTable_d(f), d, ignore_attr = TRUE)
  expect_equal(f$dic$description, dic$description)

  dic <- data.frame(label = c("Speed", "Dist"),
                    id = c("speed", "dist"),
                    hdType = c("Num", "Cat"),
                    stringsAsFactors = FALSE)
  f2 <- hdTable(d, dic = dic)
  expect_equivalent(hdTable_d(f2)[[2]], as.character(cars[[2]]))
  expect_equivalent(names(hdTable_data(f2)), dic$id)
  expect_equivalent(names(hdTable_data(f2, labels =TRUE)), dic$label)

  dic <- data.frame(label = c("speed", "dist"),
                    id = c("speed", "dist"),
                    description = c("SPEED", "DIST"))
  f3 <- hdTable(d, dic = dic)
  expect_equal(f3, f)

  #expect_equal(as.character(f3$hdTableTypeGroup), hdTableType(f3))

})



test_that("hdTable dictionaries have correct format",{

  f <- sample_hdTable("Cat-Dat-Num-Pct", n = 11,
                     names = c("Category", "Dates", "Numbers","Percentages"))
  f$data
  f$d()

  dd <- hdTable_d(f)
  expect_equal(names(dd), c('a','b','c','d'))
  expect_equal(purrr::map_chr(dd, class),
               c(a = "character", b = "Date", c = "numeric", d = "numeric"))
  expect_true(all(!purrr::map_lgl(dd, is_hdType)))


  # New hdTable with dic

  data <- data.frame(book = c("Black", "Red"),
                     value = 1:2,
                     dates = c("28/04/2019", "4/12/2018"))
  dic <- data.frame(id = names(data),
                    hdType = c("Cat","Num","Dat"))
  f <- hdTable(data, dic = dic)

  expect_equal(dic$id, f$dic$id)
  expect_equivalent(hdType(dic$hdType), f$dic$hdType)

  f_data <- hdTable_data(f)
  expect_false("hd_tbl" %in% class(f_data))

  f_hdTibble <- hdTable_hdTibble(f)
  expect_true("hd_tbl" %in% class(f_hdTibble))

  #

  data <- data.frame(
    a = Cat(c("black", "white")),
    b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
    c = Yea(2001:2002),
    d = Num(runif(2)*10),
    e = Pct(runif(2))
  )
  class(data)
  f <- hdTable(data)
  f_data <- hdTable_data(f)

  expect_false("hd_tbl" %in% class(f_data))

  f_hdTibble <- hdTable_hdTibble(f)
  expect_true("hd_tbl" %in% class(f_hdTibble))

})

test_that("hdTable", {

  f0 <- hdTable(mtcars, name = "Mtcars", access = "private")
  expect_equal(f0$name, "Mtcars")

  f1 <- hdTable_update_meta(f0, name = "MTCARS 2")

  is_hdTable(f1)

  expect_equal(f1$name, "MTCARS 2")
  expect_equal(f1$slug, "mtcars")
  expect_equal(f1$meta$access, "private")
  expect_equal(f1$meta$access, "private")

  f2 <- hdTable_update_meta(f1, name = "Mtcars", slug="new_mtcars")
  f3 <- hdTable_update_meta(f0, slug = "new_mtcars")
  expect_equal(f3, f2)

  sources <- list(title = "source name", path = "url-of-source")

  f4 <- hdTable(mtcars, sources = sources)
  expect_equal(f4$meta$sources, sources)

  sources_update <- list()
  sources_update[[1]] <- sources
  sources_update[[2]] <- list(title = "another source", path = "url-of-source")

  f5 <- hdTable_update_meta(f4, name = "this data", sources = sources_update)
  expect_equal(f5$meta$sources, sources_update)
  expect_equal(f5$name, "this data")

})

