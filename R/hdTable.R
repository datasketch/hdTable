
#' @title Create a hdtable data frame
#' @description Create a hdtable object from a data frame. The main value of a hdtable is its metadata. When creating it, hdtable will add to the data frame the following information:
#'
#' - data: original data frame data. When it is created, the hdtable will convert original variable R types onto homodatum ones (Num, Cat, Pct, etc. -see [available_hdtypes()])
#' - dic: A diccionary is created with three variable characteristics: id, label and hdtype
#' - hdtableType: Shows all variable types based in homodatum schema
#' - group: A grouped view of hdtableType
#' - name: Name for the hdtable data frame, setted on _name_ argument
#' - description: Description for the hdtable data frame, setted on _description_ argument
#' - slug: a custom slug can be added
#' - stats: Depending on the variable type given by homodatum, the hdtable will generate different kind of statistics: nrow, ncol, n_unique, n_na, pct_na, min, max
#' @param x A data frame
#' @param hdtableType The type of hdtable to create
#' @param dic a custom variable dictionary can be added. [create_dic()] can help you with that.
#' @param name a custom name can be added
#' @param nam a custom description can be added
#' @param slug a custom slug can be added. If not, hdtable will try creating one.
#' @param meta Custom Metadata can be added
#'
#' @examples
#' hdtable(mtcars, hdtableType = "Num", name = "MTCars")
#'
#' @return A hdtable object
#' @export
hdtable <- function(d,
                    dic = NULL,
                    hdtableType = NULL,
                    name = NULL,
                    description = NULL,
                    slug = NULL,
                    meta = NULL,
                    formats = NULL,
                    ...){

  if(is_hdtable(d)) return(d)

  name <- name %||% deparse(substitute(d))
  meta <- c(meta, list(...))
  if(dstools::is.empty(meta)) meta <- NULL

  hdtableClass$new(d, dic = dic, hdtableType = hdtableType,
               name = name, description = description,
               slug = slug, meta = meta, formats = formats)
}




#' @title hdtable data frame
#' @description test for objects of type "hdtable"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hdtable or not.
#'
#' @examples
#' some_df <- hdtable(mtcars)
#' is_hdtable(some_df)
#'
#' @export
is_hdtable <- function(x) {
  inherits(x, "hdtable")
}





#' @export
hdtable_update_meta <- function(f, ...){
  fixed <- c("data", "dic", "hdtableType", "hdtableGroupType")
  message("args")
  args <- list(...)
  if(any(names(args) %in% fixed)){
    warning("Cannot update ",
            paste0(names(args)[names(args) %in% fixed], collapse = ", "),
            ". Removing from meta.")
    args <- args[!names(args) %in% fixed]
  }

  f$name <- args$name %||% f$name
  f$description <- args$description %||% f$description
  f$slug <- args$slug %||% f$slug
  meta <- args[!names(args) %in% c("name", "description","slug")]
  common_names <- intersect(names(f$meta), names(meta))
  # Delete info from common names
  purrr::walk(common_names, function(nm){
    message(nm)
   f$meta[[nm]] <- NULL
  })
  updated_meta <- purrr::list_modify(list(f$meta), meta)[[1]]
  f$meta <- updated_meta
  f
}




#' @export
hdtable_labels <- function(f){
  labels <- f$dic$label
  names(labels) <- letterNames(f$stats$ncol)
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
hdtable_hdtableType <- function(fr){
  as.character(fr$hdtableType)
}




#' #' @export
#' force_hdtypes <- function(df, hdtypes){
#'   df <- as.data.frame(df)
#'   if(ncol(df)!= length(hdtypes)) stop("number of df cols must be the same as col types length")
#'   for (i in seq_along(hdtypes)){
#'     if(hdtypes[i]=="Num"){df[,i]<- as.numeric(df[,i])}
#'     if(hdtypes[i]=="Yea"){df[,i]<- as.character(df[,i])}
#'     if(hdtypes[i]=="Cat"){df[,i]<- as.character(df[,i])}
#'     if(hdtypes[i]=="Txt"){df[,i]<- as.character(df[,i])}
#'     if(hdtypes[i]=="Img"){
#'       if(!isImgUrl(df[,i])) stop ("Not an image Url")
#'       df[,i]<- as.character(df[,i])
#'     }
#'     if(hdtypes[i]=="Dat"){df[,i]<- parseDatetime(df[,i],"Dat")}
#'     if(hdtypes[i]=="Hms"){df[,i]<- parseDatetime(df[,i],"Hms")}
#'     if(hdtypes[i]=="Dti"){df[,i]<- parseDatetime(df[,i],"Dti")}
#'     if(hdtypes[i]=="Glt"){df[,i]<- as.numeric(df[,i])}
#'     if(hdtypes[i]=="Gln"){df[,i]<- as.numeric(df[,i])}
#'     if(hdtypes[i]=="Gnm"){df[,i]<- as.character(df[,i])}
#'   }
#'   as_tibble(df)
#' }


