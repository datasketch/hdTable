



#' @export
is_hdtibble <- function(d){
  all(purrr::map_lgl(d, hdtype::is_any_hdtype))
}

#' @export
hdtibble <- function(d, dic = NULL){

  if(is_hdtibble(d)) return(d)
  if(is.null(dic)){
    dic <- create_dic(d)
  }


  hdts <- dic$hdtype
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



hdtibble_frType <- function(d){
  frType(hdtype(purrr::map_chr(d, which_hdtype)))
}



#' @export
hdtibble_hdtypes <- function(d){
  hdtype(purrr::map_chr(d, which_hdtype))
}



