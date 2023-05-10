#'
#'
#'
#'
#' #' @export
#' is_hdTable <- function(d){
#'   all(purrr::map_lgl(d, hdField::is_any_hdField))
#' }
#'
#' #' @export
#' hdTable <- function(df, hdTableType = NULL){
#'
#'   if(is.null(hdTableType)){
#'     hdTableType <- guess_hdTableType(df)
#'   }
#'   if(!is_hdTableType(hdTableType)){
#'     hdTableType <- hdTableType(hdTableType)
#'   }
#'   hdtypes <- hdTableType_hdTypes(hdTableType)
#'   hdtypes_str <- vctrs::vec_data(hdtypes)
#'
#'   df <- as.data.frame(df)
#'   # HERE GO ALL CASTS WITH GIVEN hdTableType
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
#' hdTable_hdTableType <- function(d){
#'   hdTableType(hdType(purrr::map_chr(d, which_hdType)))
#' }
#'
#'
#'
#' #' @export
#' hdTable_hdTypes <- function(d){
#'   hdType(purrr::map_chr(d, which_hdType))
#' }
#'
#'
#'
