


#' @export
hdtable_labels <- function(f){
  labels <- f$dic$label
  labels
}

#' @export
hdtable_ids <- function(f){
  f$dic$id
}







#' @export
hdtable_column <- function(f, column){
  idx <- NULL
  if(is.numeric(column)){
    idx <- column
  }else{
    if(is.character(column)){
      idx <- match(column, hdtable_labels(f))
      if(is.na(idx)){
        idx <- match(column, hdtable_ids(f))
      }
      if(column %in% letters){
        d <- hdtable_d(f)
        return(d[[column]])
      }
    }
  }
  if(is.null(idx)) stop("column not found")
  hdtable_d(f)[[idx]]
}


#'@export
hdtable_d <- function(f){
  f$d()
}

#'@export
hdtable_data <- function(f, labels = FALSE){
  data <- hdtable_d(f)
  if(labels){
    names(data) <- hdtable_dic(f)$label
  }else{
    names(data) <- hdtable_dic(f)$id
  }
  class(data) <- class(data)[class(data) != "hd_tbl"]
  data
}

#'@export
hdtable_hdtibble <- function(f){
  data <- f$data
  if(!"hd_tbl" %in% class(data)){
    class(data) <- c(class(data), "hd_tbl")
  }
  data
}

#' @export
hdtable_dic <- function(f, id_letters = FALSE, stats = FALSE){
  dic <- f$dic
  if(id_letters)
    dic$id_letters <- letterNames(nrow(dic))
  if(stats){
    col_stats <- hdtable_stats(f)$col_stats
    dic$stats <- unname(col_stats)
  }
  dic
}

#'@export
hdtable_hdtypes <- function(fr, named = FALSE){
  x <- fr$dic$hdtype
  if(named) names(x) <- fr$dic$id
  x
}

#'@export
hdtable_hdtable_type <- function(fr){
  as.character(fr$hdtable_type)
}







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
