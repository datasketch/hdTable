

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
  dic

}
