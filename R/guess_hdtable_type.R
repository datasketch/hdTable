
#' @export
guess_hdtable_type <- function(data, as_string = FALSE){
  hdtypes <- purrr::map_chr(data, hdtype::guess_hdtype)
  out <- hdtable_type(paste(hdtypes, collapse = "-"))
  if(as_string){
    out <- vctrs::vec_data(out)
  }
  out
}

