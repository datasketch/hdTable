
#' @export
guess_hdtableType <- function(data, as_string = FALSE){
  hdtypes <- purrr::map_chr(data, hdtype::guess_hdtype)
  out <- hdtableType(paste(hdtypes, collapse = "-"))
  if(as_string){
    out <- vctrs::vec_data(out)
  }
  out
}

