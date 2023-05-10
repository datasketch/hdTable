test_that("Fringe IO works", {

  d <- tibble::tibble("Helloo X" = 1, "x 43" = as.Date("2020-04-21"))
  hdtab <- hdTable(d, name = "Los Carros", mas = "fda")
  expect_true(hdtab$meta$mas == "fda")
  expect_true(hdtab$slug == "los-carros")

  dir <- tempdir(check = TRUE)
  # dir <- "tmp"
  ### for some reason this tempdir clashes with write_csv(data, "")

  hdTable_write(hdtab, path = dir, overwrite_dic = TRUE)
  hdTable_write_json(hdtab, path = dir)
  hdTable_write_xlsx(hdtab, path = dir)

  expect_true(all(file.exists(file.path(dir,
                                        c('los-carros.csv',
                                          'los-carros.dic.csv',
                                          'los-carros.json',
                                          'los-carros.meta.json',
                                          'los-carros.xlsx',
                                          'los-carros.yaml')))))

  path <- file.path(dir, hdtab$slug)

  hdtab2 <- hdTable_read(path)

  expect_equal(hdtab$data, hdtab2$data)
  expect_equal(hdtab$dic, hdtab2$dic)
  expect_equal(hdtab$meta, hdtab2$meta)
  # expect_equal(fr, fr2)

  unlink(dir, recursive = TRUE)

})
