

#' @title Dictionary
#' @description `create_dic()` Creates a data.frame dictionary identifying column id (with cleaned variable names), label and homodatum variable type
#'
#' @param d The data set for which the user is creating the dictionary for
#' @param hdtable_type pre-defined fringe types (check available_hdtypes() for the complete list)
#'
#' @return a data frame with five columns: id, label, hdtype, format and stats
#' @export
#'
#' @examples
#' d <- mtcars
#' new_dic <- create_dic(d)
create_dic <- function(d, hdtable_type = NULL){
  if(is.null(d)) return()

  if("rcd___id" %in% names(d)){
    d$rcd___id <- NULL
  }

  if(is.null(hdtable_type)){
    if(is_hdtable(d)){
      hdtable_type <- hdtable_hdtable_type(d)
    }else{
      hdtable_type <- guess_hdtable_type(d)
    }
  }
  if(!is_hdtable_type(hdtable_type)){
    hdtable_type <- hdtable_type(hdtable_type)
  }
  ids <- clean_names(names(d))

  dic <-tibble::tibble(id = ids, label = names(d),
                       hdtype = hdtable_type_hdtypes(hdtable_type))
  dic <- update_dic(dic, d)
  dic$fld___id <- random_id_vector(nrow(dic))
  dic

}


#' @title Update Dictionary with data
#' @description `update_dic()` Creates a data.frame dictionary identifying column
#' id (with cleaned variable names), label and homodatum variable type
#'
#' @param dic The original dictionary
#' @param d The data set for which the user is creating the dictionary for
#'
#' @return a data frame with five columns: id, label, hdtype, format and stats
#' @export
#'
#' @examples
#' d <- mtcars
#' new_dic <- create_dic(d)
update_dic <- function(dic, d, stats = TRUE){

  nms_d <- names(d)[names(d) != "rcd___id"]
  if(! (all(nms_d == dic$id) || all(names(d) == dic$label))){
      stop("Names of data do not correspond to dictionary columns ids or labels")
  }

  if(inherits(d, "turn_table") || inherits(d, "turn_tables")){
    d_class <- class(d)
    class(d) <- d_class[!d_class %in% c("turn_table", "turn_tables")]
  }

  d <- d |> dplyr::select(-any_of("rcd___id"))
  # Update format and stats
  dic$format <- get_fields(d, dic, "format")
  if(stats){
    dic$stats <- get_fields(d, dic, "stats")
  }
  # Keeps the same rcd_ids from before

  if("fld___id" %in% names(dic)){
    dic <- dic |> dplyr::relocate(fld___id, .after = last_col())
  }

  dic

}




get_fields <- function(d, dic, what = "format"){
  # what can by 'format' or 'stats'
  names(d) <- dic$id
  d <- hdtibble(d, dic = dic)
  if("rcd___id" %in% names(d)) d$rcd___id <- NULL
  purrr::map2(d, dic$hdtype, function(field, hdtype){
    fun_str <- paste0("hdtype::",hdtype, "_", what)
    do.call(getfun(fun_str), list(field))
  })

}


# https://stackoverflow.com/questions/38983179/do-call-a-function-in-r-without-loading-the-package
getfun <- function(x) {
  if(length(grep("::", x)) > 0) {
    parts <- strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    x
  }
}
