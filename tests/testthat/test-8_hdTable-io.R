test_that("Fringe IO works", {

  d <- tibble::tibble("Helloo X" = 1, "x 43" = as.Date("2020-04-21"))
  hdtab <- hdTable(d, name = "Los Carros", mas = "fda", formats = "xlsx")


  expected_write_ext <- c(".csv", ".dic.csv", ".dic.json", ".json",
                          ".meta.json", ".xlsx")



  hdtab$write_csv("tmp/tmp2")
  hdtab$write_meta_json("tmp/tmp2")
  hdtab$write_json("tmp/tmp2")
  hdtab$write_xlsx("tmp/tmp2")

  hdtab$write("tmp/tab1")
  expect_equal(list.files("tmp/tab1"), paste0(hdtab$slug, expected_write_ext))

  hdTable_write(hdtab, "tmp/tab2")

  expect_equal(list.files("tmp/tab1"), list.files("tmp/tab2"))

  unlink("tmp/tab1", recursive = TRUE)
  unlink("tmp/tab2", recursive = TRUE)

  expect_equal(hdtab$available_write_formats(), c("xlsx", "json", "csv"))




  # Bigger to test preview


  expected_write_ext_with_preview <- c(".csv", ".dic.csv", ".dic.json", ".json",
                          ".meta.json", ".preview.json")
  tib <- tibble::tibble(n = 1:20)
  ht <- hdTable(tib)
  ht$preview_max_nrow <- 10
  ht$write("tmp/larger")
  expect_equal(list.files("tmp/larger"),
               paste0(ht$slug, expected_write_ext_with_preview))
  prev <- jsonlite::read_json("tmp/larger/tib.preview.json", simplifyVector = TRUE)
  nrow(prev)
  expect_equal(nrow(prev), ht$preview_max_nrow)
  unlink("tmp/larger", recursive = TRUE)


  # Another

  dir <- "tmp/tab2"
  ### for some reason this tempdir clashes with write_csv(data, "")


  hdtab$write_csv("tmp/tab2")
  hdtab$write_meta_json("tmp/tab2")
  hdtab$write_json("tmp/tab2")

  # Ensure format and stats are saved in dic.json, not in dic.csv
  l <- jsonlite::read_json("tmp/tab2/los-carros.dic.json")
  l <- purrr::transpose(l)
  expect_true(!is.null(l$format))
  expect_true(!is.null(l$stats))

  hdtab$write_xlsx("tmp/tab2")

  expected_files <- c('los-carros.csv', 'los-carros.dic.csv',
                      'los-carros.json', 'los-carros.dic.json', 'los-carros.meta.json',
                      'los-carros.xlsx')

  expect_true(all(file.exists(file.path(dir,expected_files))))




  path <- file.path(dir)


  hdtab2 <- hdTable_read(path)

  expect_equal(hdtab$data, hdtab2$data)
  expect_equal(hdtab$dic, hdtab2$dic)
  expect_equal(hdtab$meta, hdtab2$meta)

  unlink("tmp/tab2", recursive = TRUE)

})
