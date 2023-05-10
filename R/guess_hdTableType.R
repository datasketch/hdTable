
#' @export
guess_hdTableType <- function(data, as_string = FALSE){
  hdtypes <- purrr::map_chr(data, hdTypes::guess_hdType)
  out <- hdTableType(paste(hdtypes, collapse = "-"))
  if(as_string){
    out <- vctrs::vec_data(out)
  }
  out
}

