
#' @export
valid_dic_ids <- function(dic, d = NULL){

  if(!is.null(d)){
    nms <- names(d)
    nms <- nms[nms != "rcd___id"]
    # Table names and dic ids are consistent
    d_nms_ok <- all(nms %in% dic$id)
    if(!d_nms_ok){
      message("missing dic id: ", dstools::which_not_in(nms, dic$id))
      return(FALSE)
    }
    dic_nms_ok <- all(dic$id %in% nms)
    if(!dic_nms_ok){
      message("missing column: ", dstools::which_not_in(dic$id, nms))
      return(FALSE)
    }
  }

  # names are unique
  unique_names <- length(dic$id) == length(unique(dic$id))
  if(!unique_names) return(FALSE)

  # Table names consist only on lowercase letters numbers and underscores
  x <- dic$id
  no_numbers_underscore <- gsub("[0-9_]", "", x)
  any_upper_case <- any(grepl("[A-Z]", no_numbers_underscore))
  if(any_upper_case) return(FALSE)

  # Cannot start with numbers or start of end in underscore
  start_number_underscore <- any(grepl("^[0-9_]", x, perl = TRUE))
  if(start_number_underscore) return(FALSE)

  # Cannot end in number or underscore
  end_underscore <- any(grepl("[_]$", x, perl = TRUE))
  if(end_underscore) return(FALSE)

  TRUE
}

