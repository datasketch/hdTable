test_that("hdtable", {

  d <- hdtable(NULL)
  expect_null(d$data)
  expect_null(d$dic)


  # NA hdtables
  d <- data.frame(x = NA)
  dic <- create_dic(data.frame(x = NA))
  dd <- hdtibble(d, dic = dic)
  expect_equal(UKT(d$x), dd$x)
  #expect_equal(as.logical(dd$x), NA) # Casting to logical not working

  d <- data.frame(x = NA)
  dic <- create_dic(d)
  hdtibble(d, dic)
  hdtab <- hdtable(d)

  hdtab$dd
  hdtab$df()
  hdtab$data

  f <- hdtable(cars)
  length(f) # 27??? ----> fields in the class

  # check fld_id created with 8 random characters and numbers
  expect_true(all(grepl("^\\b[a-zA-Z0-9]{8}\\b$",f$dic$fld___id))) # check they have 8 letters

  fr <- hdtable(cars)
  expect_equal(f$data, fr$data)
  expect_equal(f$dic |> dplyr::select(-fld___id),
               fr$dic |> dplyr::select(-fld___id))
  # expect_equal(f$frtype, fr$frtype) ### TODO
  expect_equal(f$group, fr$group)

  f2 <- list(hdtable(mtcars), hdtable(cars))
  expect_true(inherits(hdtable(cars),"hdtable"))


  ### SAMPLE DATA

  d <- sample_data("Cat-Dat-Num-Pct", n = 11,
                   names = c("Category", "Dates", "Numbers","Percentages"))
  names(d)
  f <- hdtable(d)

  f$dic
  f$dd
  f$df()

  expect_equal(d, f$df(), ignore_attr = TRUE)
  expect_equal(f$metadata()$nrow, 11)
  expect_equal(f$metadata()$ncol, 4)

})

test_that("hdtable creation with dictionaries work",{

  d <- cars
  names(d) <- c("a", "b")

  dic <- data.frame(label = c("speed", "dist"),
                    id = c("a", "b"),
                    description = c("SPEED", "DIST"),
                    hdtype = c("Num", "Num"))
  f <- hdtable(d, dic = dic)
  expect_equal(hdtable_df(f), d, ignore_attr = TRUE)
  expect_equal(f$dic$description, dic$description)

  dic <- data.frame(label = c("Speed", "Dist"),
                    id = c("speed", "dist"),
                    hdtype = c("Num", "Cat"),
                    stringsAsFactors = FALSE)
  expect_error(hdtable(d, dic = dic))

  names(d) <- c("speed", "dist")
  f2 <- hdtable(d, dic = dic)


  expect_equal(hdtable_data(f2)[[2]], as.character(cars[[2]]), ignore_attr = TRUE)
  expect_equal(names(hdtable_data(f2)), dic$id, ignore_attr = TRUE)


  dic <- data.frame(label = c("speed", "dist"),
                    id = c("speed", "dist"),
                    description = c("SPEED", "DIST"))
  f3 <- hdtable(d, dic = dic)
  expect_equal(f3$name, f$name)


})



test_that("hdtable dictionaries are correct",{

  f <- sample_hdtable("Cat-Dat-Num-Pct", n = 11,
                      names = c("Category", "Dates", "Numbers","Percentages"))
  f$data
  f$df()

  dd <- hdtable_data(f)
  nms <- dstools::create_slug(c("Category", "Dates", "Numbers","Percentages"))
  expect_equal(names(dd), nms)
  expect_equal(purrr::map_chr(dd, class),
               c("category" = "character", "dates" = "Date",
                 "numbers" = "numeric", "percentages" = "numeric"))
  expect_true(all(!purrr::map_lgl(dd, is_hdtype)))


  # New hdtable with dic

  data <- data.frame(book = c("Black", "Red"),
                     value = 1:2,
                     dates = c("28/04/2019", "4/12/2018"))
  dic <- data.frame(id = names(data),
                    hdtype = c("Cat","Num","Dat"))
  f <- hdtable(data, dic = dic)

  expect_true(!is.null(f$dic$label))

  expect_equal(dic$id, f$dic$id)
  expect_equal(hdtype(dic$hdtype), f$dic$hdtype)

  f_data <- hdtable_data(f)
  expect_false("hd_tbl" %in% class(f_data))

  f_hdtibble <- hdtable_hdtibble(f)
  expect_true("hd_tbl" %in% class(f_hdtibble))

  #

  data <- data.frame(
    a = Cat(c("black", "white")),
    b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
    c = Yea(2001:2002),
    d = Num(runif(2)*10),
    e = Pct(runif(2))
  )
  class(data)
  f <- hdtable(data)
  f_data <- hdtable_data(f)

  expect_false("hd_tbl" %in% class(f_data))

  f_hdtibble <- hdtable_hdtibble(f)
  expect_true("hd_tbl" %in% class(f_hdtibble))

})



test_that("hdtable dictionaries have format",{

  f <- sample_hdtable("Cat-Dat-Num-Pct", n = 11,
                      names = c("Category", "Dates", "Numbers","Percentages"))

  f$dic

  dd <- hdtable_data(f)
  nms <- dstools::create_slug(c("Category", "Dates", "Numbers","Percentages"))
  expect_equal(names(dd), nms)
  expect_equal(purrr::map_chr(dd, class),
               c("category" = "character", "dates" = "Date",
                 "numbers" = "numeric", "percentages" = "numeric"))
  expect_true(all(!purrr::map_lgl(dd, is_hdtype)))


  # New hdtable with dic

  data <- data.frame(book = c("Black", "Red"),
                     value = 1:2,
                     dates = c("28/04/2019", "4/12/2018"))
  dic <- data.frame(id = names(data),
                    hdtype = c("Cat","Num","Dat"))
  f <- hdtable(data, dic = dic)

  expect_equal(dic$id, f$dic$id)
  expect_equal(hdtype(dic$hdtype), f$dic$hdtype)

  f_data <- hdtable_data(f)
  expect_false("hd_tbl" %in% class(f_data))

  f_hdtibble <- hdtable_hdtibble(f)
  expect_true("hd_tbl" %in% class(f_hdtibble))

  #

  data <- data.frame(
    a = Cat(c("black", "white")),
    b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
    c = Yea(2001:2002),
    d = Num(runif(2)*10),
    e = Pct(runif(2))
  )
  class(data)
  f <- hdtable(data)
  f_data <- hdtable_data(f)

  expect_false("hd_tbl" %in% class(f_data))

  f_hdtibble <- hdtable_hdtibble(f)
  expect_true("hd_tbl" %in% class(f_hdtibble))

})

test_that("hdtable works with an hdtibble with rcd___id", {

  d <- data.frame(book = c("Black", "Red"),
                   value = 1:2,
                   dates = c("28/04/2019", "4/12/2018"),
                   rcd___id = c("45kjlkj4", "068kjlkj"))
  guess_hdtable_type(d)
  create_dic(d)
  hdtibble <- hdtibble(d)



  expect_equal(hdtibble$rcd___id, d$rcd___id)

})


test_that("hdtable with metadata", {

  d <- tibble::tibble("Helloo X" = 1, "x 43" = as.Date("2020-04-21"))
  hdtab <- hdtable(d, name = "Los Carros", mas = "fda")
  expect_equal(hdtab$formats, c("csv", "json"))


  d <- tibble::tibble("Helloo X" = 1, "x 43" = as.Date("2020-04-21"))
  hdtab <- hdtable(d, name = "Los Carros", mas = "fda", formats = "xlsx")
  expect_true(hdtab$meta$mas == "fda")
  expect_true(hdtab$slug == "los-carros")
  expect_equal(hdtab$formats, c("csv", "json", "xlsx"))
  expect_equal(hdtab$formats, hdtab$metadata()$formats)




  f0 <- hdtable(mtcars, name = "Mtcars", access = "private")
  expect_equal(f0$name, "Mtcars")

  f1 <- hdtable_update_meta(f0, name = "MTCARS 2")

  is_hdtable(f1)

  expect_equal(f1$name, "MTCARS 2")
  expect_equal(f1$slug, "mtcars")
  expect_equal(f1$meta$access, "private")
  expect_equal(f1$meta$access, "private")

  f2 <- hdtable_update_meta(f1, name = "Mtcars", slug="new_mtcars")
  f3 <- hdtable_update_meta(f0, slug = "new_mtcars")
  expect_equal(f3, f2)

  sources <- list(title = "source name", path = "url-of-source")

  f4 <- hdtable(mtcars, sources = sources, license = "MIT")

  expect_equal(f4$meta$sources, sources)
  expect_equal(f4$meta$license, "MIT")

  sources_update <- list()
  sources_update[[1]] <- sources
  sources_update[[2]] <- list(title = "another source", path = "url-of-source")

  f5 <- hdtable_update_meta(f4, name = "this data",
                            sources = sources_update,
                            more = "Más info")
  f5$meta
  expect_equal(f5$meta$more, "Más info")
  expect_equal(f5$meta$sources, sources_update)
  expect_equal(f5$name, "this data")

})

test_that("hdtable getters work", {

  hdt <- hdtable(tibble::tibble(z = 1, a = "A", b = 0))
  expect_equal(as.character(hdtable_type(hdt)), "Num-Cat-Num")
  expect_equal(as.character(hdtable_type_group(hdt)), "Cat-Num2")


})





