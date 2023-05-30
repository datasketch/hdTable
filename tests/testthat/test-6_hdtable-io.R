test_that("Fringe IO works", {

  d <- tibble::tibble("Helloo X" = 1,
                      "x 43" = as.Date("2020-04-21"),
                      "Cats" = "Michi")
  hdtab <- hdtable(d, name = "Los Carros", mas = "fda", formats = "xlsx")


  expected_write_ext <- c(".csv", ".dic.csv", ".dic.json", ".json",
                          ".meta.json", ".xlsx")

  hdtab$dd # hdtibble with labels and rcd___id

  hdtab$df_slug_rcd() # data.frame with slug and rcd___id
  hdtab$df() # data.frame with labels
  hdtab$data # data.frame with slug


  hdtab$write_csv("tmp/tmp2")
  hdtab$write_meta_json("tmp/tmp2")
  hdtab$write_json("tmp/tmp2")

  json <- jsonlite::read_json("tmp/tmp2/los-carros.json", simplifyVector = TRUE)
  str(json)
  expect_equal(col_ids_from_name(names(d)),
               names(json |> dplyr::select(-rcd___id)))
  expect_true(!is.null(json$rcd___id))

  expect_true(lubridate::is.Date(as.Date(json$x_43)))

  hdtab$write_xlsx("tmp/tmp2")

  hdtab$write("tmp/tab1")
  expect_equal(list.files("tmp/tab1"), paste0(hdtab$slug, expected_write_ext))

  hdtable_write(hdtab, "tmp/tab2")

  expect_equal(list.files("tmp/tab1"), list.files("tmp/tab2"))

  unlink("tmp/tab1", recursive = TRUE)
  unlink("tmp/tab2", recursive = TRUE)

  expect_equal(hdtab$available_write_formats(), c("xlsx", "json", "csv"))

})

test_that("Big files write preview",{
  # Bigger to test preview

  dir.create("tmp/larger", recursive = TRUE, showWarnings = FALSE)

  expected_write_ext_with_preview <- c(".csv", ".dic.csv", ".dic.json",
                                       ".meta.json", ".preview.json")
  m <- matrix(rep(0,100000), ncol = 100, nrow = 1000) |> as.data.frame()
  ht <- hdtable(m)
  ht$magnitude

  ht$write("tmp/larger")
  expect_equal(list.files("tmp/larger"),
               paste0(ht$slug, expected_write_ext_with_preview))
  prev <- jsonlite::read_json("tmp/larger/m.preview.json", simplifyVector = TRUE)
  expect_equal(nrow(prev), ht$preview_max_nrow)

  unlink("tmp/larger", recursive = TRUE)

})


test_that("Write Read tables",{

  dir.create("tmp/mas", recursive = TRUE, showWarnings = FALSE)

  d <- tibble::tibble("Helloo X" = 1,
                      "x 43" = as.Date("2020-04-21"),
                      "Cats" = "Michi")
  hdtab <- hdtable(d, name = "Más carros", mas = "fda", formats = "xlsx")

  dir <- "tmp/mas"
  hdtab$write_csv("tmp/mas")
  hdtab$write_meta_json("tmp/mas")
  hdtab$write_json("tmp/mas")
  hdtab$write_xlsx("tmp/mas")

  l <- jsonlite::read_json("tmp/mas/mas-carros.dic.json")
  l <- purrr::transpose(l)
  expect_true(!is.null(l$format))
  expect_true(!is.null(l$stats))

  expected_files <- c('mas-carros.csv', 'mas-carros.dic.csv',
                      'mas-carros.json', 'mas-carros.dic.json',
                      'mas-carros.meta.json',
                      'mas-carros.xlsx')

  expect_true(all(file.exists(file.path(dir,expected_files))))

  path <- file.path(dir)

  hdtab2 <- hdtable_read(path, lazy = FALSE)
  expect_false(hdtab2$lazy)


  expect_equal(hdtab$data, hdtab2$data)
  expect_equal(hdtab$dic,
               hdtab2$dic)
  expect_equal(hdtab$meta, hdtab2$meta)
  expect_equal(hdtab, hdtab2)



  hdtab3 <- hdtable_read(path, lazy = TRUE)
  expect_null(hdtab3$dd)
  expect_true(hdtab3$lazy)
  expect_equal(hdtab3$data, hdtab2$data)

  unlink("tmp/mas", recursive = TRUE)


})


test_that("Read write gives the same results",{

  d1 <- head(iris)
  d1 <- hdtable(d1, name = "Iris")

  path <- "tmp/iris"

  hdtable_write(d1, path)

  d2 <- hdtable_read(path, lazy = FALSE)
  d2 <- hdtable_read(path, lazy = TRUE)

  expect_equal(d1$data, d2$data)
  expect_equal(d1$df_slug_rcd(), d2$df_slug_rcd())

  unlink(path, recursive = TRUE)



})


test_that("Save and read some values", {


  with_na <- tibble::tibble(a = 1:5, z = rep(NA,5))
  h <- hdtable(with_na)
  hdtable_write(h, "tmp/with_na")

  h2 <- hdtable_read("tmp/with_na")
  expect_true(all(is.na(h2$dd$z)))



  # Test preview with larger columns
  # matrix <- tibble::as_tibble(matrix(1:120, nrow = 5))
  # names(matrix) <- paste("Ho ", names(matrix))
  # h <- hdtable(matrix)
  # h$ncol
  # h$nrow
  #
  # h$write_json("tmp/matrix")
  # hdtable_write(h, "tmp/matrix2")
  #
  # hdtable_read("tmp/matrix2")


})


test_that("Large files read", {

  path <- "tmp/large_files"
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  file_path_rand <- file.path(path, "rand.csv")
  file_path_rand_dic <- file.path(path, "rand.dic.csv")

  # Create large table
  rand <- rep(dstools::random_name(100000), 10000)
  rand <- tibble::tibble(rand = rand, n = 1:length(rand))
  dic_rand <- tibble::tibble(id = c("rand", "n"),
                             label = c("RAND", "N"),
                             hdtype = c("Cat", "Num"))
  vroom::vroom_write(rand, file_path_rand)
  vroom::vroom_write(dic_rand, file_path_rand_dic)

  h1 <- hdtable_read(path)
  h2 <- hdtable_read(path, slug = "rand")
  expect_null(h1$dd)
  expect_null(h2$dd)
  expect_true(h1$lazy == h2$lazy)
  expect_equal(h1$d_path, h2$d_path)

  ## TODO FIX rcd___id, maybe not reading it


})









