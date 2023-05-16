#'
#'
#'
#'
#' #' @export
#' is_hdtable <- function(d){
#'   all(purrr::map_lgl(d, hdField::is_any_hdField))
#' }
#'
#' #' @export
#' hdtable <- function(df, hdtableType = NULL){
#'
#'   if(is.null(hdtableType)){
#'     hdtableType <- guess_hdtableType(df)
#'   }
#'   if(!is_hdtableType(hdtableType)){
#'     hdtableType <- hdtableType(hdtableType)
#'   }
#'   hdtypes <- hdtableType_hdtypes(hdtableType)
#'   hdtypes_str <- vctrs::vec_data(hdtypes)
#'
#'   df <- as.data.frame(df)
#'   # HERE GO ALL CASTS WITH GIVEN hdtableType
#'   dd <- purrr::map2(df, hdtypes_str, function(x1,y1){
#'     if(y1 == "___") return(x1)
#'     do.call(y1, list(x1))
#'   })
#'   d <- dd %>% tibble::as_tibble()
#'   class(d) <- c(class(d), "hd_tbl")
#'   d
#' }
#'
#'
#'
#' hdtable_hdtableType <- function(d){
#'   hdtableType(hdtype(purrr::map_chr(d, which_hdtype)))
#' }
#'
#'
#'
#' #' @export
#' hdtable_hdtypes <- function(d){
#'   hdtype(purrr::map_chr(d, which_hdtype))
#' }
#'
#'
#'
