



#' @export
is_hdTibble <- function(d){
  all(purrr::map_lgl(d, hdTypes::is_any_hdType))
}

#' @export
hdTibble <- function(df, dic = NULL){
  hdts <- dic$hdType
  hdts_str <- as.character(hdts)

  df <- as.data.frame(df)

  # HERE GO ALL CASTS WITH GIVEN frType
  dd <- purrr::map2(df, hdts_str, function(x1,y1){
    if(y1 == "___") return(x1)
    do.call(y1, list(x1))
  })
  d <- dd %>% tibble::as_tibble()
  class(d) <- c(class(d), "hd_tbl")
  d
}



hdTibble_frType <- function(d){
  frType(hdType(purrr::map_chr(d, which_hdType)))
}



#' @export
hdTibble_hdTypes <- function(d){
  hdType(purrr::map_chr(d, which_hdType))
}



