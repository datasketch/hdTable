



#' @export
is_hdTibble <- function(d){
  all(purrr::map_lgl(d, hdTypes::is_any_hdType))
}

#' @export
hdTibble <- function(d, dic = NULL){

  if(is.null(dic)){
    dic <- create_dic(d)
  }


  hdts <- dic$hdType
  hdts_str <- as.character(hdts)

  d <- as.data.frame(d)

  # HERE GO ALL CASTS WITH GIVEN frType
  dd <- purrr::map2(d, hdts_str, function(x1,y1){
    #if(y1 == "UKT") return(UKT(x1))
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



