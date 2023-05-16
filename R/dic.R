

#' @title Dictionary
#' @description `create_dic()` Creates a data.frame dictionary identifying column id (with cleaned variable names), label and homodatum variable type
#'
#' @param d The data set for which the user is creating the dictionary for
#' @param hdtableType pre-defined fringe types (check available_hdtypes() for the complete list)
#'
#' @return a data frame with five columns: id, label, hdtype, format and stats
#' @export
#'
#' @examples
#' d <- mtcars
#' new_dic <- create_dic(d)
create_dic <- function(d, hdtableType = NULL){
  if(is.null(d)) return()

  if(is.null(hdtableType)){
    if(is_hdtable(d)){
      hdtableType <- hdtable_hdtableType(d)
    }else{
      hdtableType <- guess_hdtableType(d)
    }
  }
  if(!is_hdtableType(hdtableType))
    hdtableType <- hdtableType(hdtableType)
  ids <- col_ids_from_name(names(d))

  dic <-tibble::tibble(id = ids, label = names(d),
                       hdtype = hdtableType_hdtypes(hdtableType))

  dic <- update_dic(dic, d)

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
update_dic <- function(dic, d){

  if(! (all(names(d) == dic$id) || all(names(d) == dic$label))){
      stop("Names of data do not correspond to dictionary columns ids or labels")
  }

  # Update format and stats
  dic$format <- get_fields(d, dic, "format")
  dic$stats <- get_fields(d, dic, "stats")

  dic

}




get_fields <- function(d, dic, what = "format"){
  # what can by 'format' or 'stats'
  names(d) <- dic$id
  d <- hdtibble(d, dic = dic)

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
