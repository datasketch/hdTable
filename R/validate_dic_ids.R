
#' @export
validate_dic_ids <- function(dic, d = NULL){

  if(!is.null(d)){
    nms <- names(d)
    nms <- nms[nms != "rcd___id"]
    # Table names and dic ids are consistent
    d_nms_ok <- all(nms %in% dic$id)
    if(!d_nms_ok){
      stop("missing dic id: ",
           dstools::collapse(dstools::which_not_in(nms, dic$id)))
    }
    dic_nms_ok <- all(dic$id %in% nms)
    if(!dic_nms_ok){
      stop("missing column: ",
           dstools::collapse(dstools::which_not_in(dic$id, nms)))
    }
  }

  # names are unique
  unique_names <- length(dic$id) == length(unique(dic$id))
  if(!unique_names) {
    stop("Repeated dic id names")
  }

  # Table names consist only on lowercase letters numbers and underscores
  x <- dic$id
  no_numbers_underscore <- gsub("[0-9_]", "", x)
  any_upper_case <- any(grepl("[A-Z]", no_numbers_underscore))
  if(any_upper_case){
    stop("All dic_id must be lowercase")
  }

  # Cannot start with numbers or start of end in underscore
  start_number_underscore <- any(grepl("^[0-9_]", x, perl = TRUE))
  if(start_number_underscore){
    stop("Dic ids cannot start with numbers or underscores")
  }

  # Cannot end in number or underscore
  end_underscore <- any(grepl("[_]$", x, perl = TRUE))
  if(end_underscore){
    stop("Dic ids cannot end with underscores")
  }

}

