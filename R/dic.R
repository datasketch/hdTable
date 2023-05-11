

#' @title Dictionary
#' @description `create_dic()` Creates a data.frame dictionary identifying column id (with cleaned variable names), label and homodatum variable type
#'
#' @param d The data set for which the user is creating the dictionary for
#' @param hdTableType pre-defined fringe types (check available_hdTypes() for the complete list)
#'
#' @return a data frame with three columns: id, label and hdType
#' @export
#'
#' @examples
#' d <- mtcars
#' new_dic <- create_dic(d)
create_dic <- function(d, hdTableType = NULL){
  if(is.null(d)) return()

  if(is.null(hdTableType)){
    if(is_hdTable(d)){
      hdTableType <- hdTable_hdTableType(d)
    }else{
      hdTableType <- guess_hdTableType(d)
    }
  }
  if(!is_hdTableType(hdTableType))
    hdTableType <- hdTableType(hdTableType)
  ids <- col_ids_from_name(names(d))

  dic <-tibble::tibble(id = ids, label = names(d),
                       hdType = hdTableType_hdTypes(hdTableType))

  field_stats <- get_fields(d, dic, "stats")

  dic

}



get_fields <- function(d, dic, what = "format"){
  # what can by 'format' or 'stats'
  names(d) <- dic$id
  d <- hdTibble(d, dic = dic)

  purrr::map2(d, dic$hdType, function(field, hdType){
    fun_str <- paste0("hdTypes::",hdType, "_", what)
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
