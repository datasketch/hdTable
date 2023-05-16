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
#' hdtable <- function(df, hdtable_type = NULL){
#'
#'   if(is.null(hdtable_type)){
#'     hdtable_type <- guess_hdtable_type(df)
#'   }
#'   if(!is_hdtable_type(hdtable_type)){
#'     hdtable_type <- hdtable_type(hdtable_type)
#'   }
#'   hdtypes <- hdtable_type_hdtypes(hdtable_type)
#'   hdtypes_str <- vctrs::vec_data(hdtypes)
#'
#'   df <- as.data.frame(df)
#'   # HERE GO ALL CASTS WITH GIVEN hdtable_type
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
#' hdtable_hdtable_type <- function(d){
#'   hdtable_type(hdtype(purrr::map_chr(d, which_hdtype)))
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
