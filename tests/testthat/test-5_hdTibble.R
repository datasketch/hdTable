test_that("hdtibble", {

  library(dplyr)
  data <- data.frame(
    a = Cat(c("black", "white")),
    b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
    c = Yea(2001:2002),
    d = Num(runif(2)*10),
    e = Pct(runif(2))
  )
  class(data)
  expect_true(is_hdtibble(data))

  d <- data.frame(x = NA)
  hdtibble(d)
  data |>  filter(a == "black")
  data |>  select(a,b)




})


test_that("tibbles with rcd___id work", {


  d <- data.frame(
    a = c("black", "white"),
    b = seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2),
    c = 2001:2002,
    d = runif(2)*10,
    e = runif(2),
    rcd___id = random_id_vector(2)
  )
  hdt <- hdtibble(d)
  is_hdtibble(hdt)
  expect_true(is_hdtibble(hdt))

  expect_equal(d$rcd___id, hdt$rcd___id)


  d <- data.frame(
    a = Cat(c("black", "white")),
    b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
    c = Yea(2001:2002),
    d = Num(runif(2)*10),
    e = Pct(runif(2)),
    rcd___id = random_id_vector(2)
  )

  expect_true(is_hdtibble(d))
  hdt2 <- hdtibble(d)
  expect_equal(d$rcd___id, hdt2$rcd___id)

})





test_that("TODO TEST TIBBLES WORK WITH DPLYR VERBS", {
  ## TODO TEST TIBBLES WORK WITH DPLYR VERBS
  # f <- fringe(data)
  # f_data <- fringe_data(f)
  # class(f_data)
  #
  # f_data %>% filter(a == "black")
  # f_data %>% select(a,b)

  # data <- tibble(
  #   a = c("black", "white"),
  #   b = seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2),
  #   c = 2001:2002,
  #   d = runif(2)*10,
  #   e = runif(2)
  # )
  # f <- fringe(data)
  # f_data <- fringe_data(f)
  #
  # f_data %>% filter(a == "black")
  # f_data %>% select(a,b)
})



