



#' @export
is_hdtibble <- function(d){
  with_rcd_id <- "rcd___id" %in% names(d)
  if(with_rcd_id){
    rcd___id <- d$rcd___id
    d$rcd___id <- NULL
  }
  all(purrr::map_lgl(d, hdtype::is_any_hdtype))
}

#' @export
hdtibble <- function(d, dic = NULL){

  if(is_hdtibble(d)) return(d)
  if(is.null(dic)){
    dic <- create_dic(d)
  }

  with_rcd_id <- "rcd___id" %in% names(d)
  if(with_rcd_id){
    rcd___id <- d$rcd___id
    d$rcd___id <- NULL
  }

  hdts <- dic$hdtype
  hdts_str <- as.character(hdts)

  names(d) <- clean_names(names(d))
  d <- as.data.frame(d)

  # HERE GO ALL CASTS WITH GIVEN hdType
  dd <- purrr::map2(d, hdts_str, function(x1,y1){
    do.call(y1, list(x1))
  })

  d <- dd |>  tibble::as_tibble()

  if(with_rcd_id){
    d$rcd___id <- rcd___id
  } else{
    d$rcd___id <- random_id_vector(nrow(d))
  }

  class(d) <- c(class(d), "hd_tbl")
  d
}


hdtibble_as_basetype <- function(d){
  purrr::map_df(d, as_basetype)
}


hdtibble_frType <- function(d){
  frType(hdtype(purrr::map_chr(d, which_hdtype)))
}



#' @export
hdtibble_hdtypes <- function(d){
  hdtype(purrr::map_chr(d, which_hdtype))
}



