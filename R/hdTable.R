
#' @title Create a hdTable data frame
#' @description Create a hdTable object from a data frame. The main value of a hdTable is its metadata. When creating it, hdTable will add to the data frame the following information:
#'
#' - data: original data frame data. When it is created, the hdTable will convert original variable R types onto homodatum ones (Num, Cat, Pct, etc. -see [available_hdTypes()])
#' - dic: A diccionary is created with three variable characteristics: id, label and hdType
#' - hdTableType: Shows all variable types based in homodatum schema
#' - group: A grouped view of hdTableType
#' - name: Name for the hdTable data frame, setted on _name_ argument
#' - description: Description for the hdTable data frame, setted on _description_ argument
#' - slug: a custom slug can be added
#' - stats: Depending on the variable type given by homodatum, the hdTable will generate different kind of statistics: nrow, ncol, n_unique, n_na, pct_na, min, max
#' @param x A data frame
#' @param hdTableType The type of hdTable to create
#' @param dic a custom variable dictionary can be added. [create_dic()] can help you with that.
#' @param name a custom name can be added
#' @param nam a custom description can be added
#' @param slug a custom slug can be added. If not, hdTable will try creating one.
#' @param meta Custom Metadata can be added
#'
#' @examples
#' hdTable(mtcars, hdTableType = "Num", name = "MTCars")
#'
#' @return A hdTable object
#' @export
hdTable <- function(d,
                    dic = NULL,
                    hdTableType = NULL,
                    name = NULL,
                    description = NULL,
                    slug = NULL,
                    meta = NULL,
                    formats = NULL,
                    ...){

  if(is_hdTable(d)) return(d)

  meta <- c(meta, list(...))
  if(dstools::is.empty(meta)) meta <- NULL

  hdTableClass$new(d, dic = dic, hdTableType = hdTableType,
               name = name, description = description,
               slug = slug, meta = meta, formats = formats)
}




#' @title hdTable data frame
#' @description test for objects of type "hdTable"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hdTable or not.
#'
#' @examples
#' some_df <- hdTable(mtcars)
#' is_hdTable(some_df)
#'
#' @export
is_hdTable <- function(x) {
  inherits(x, "hdTable")
}


calculate_hdTable_stats <- function(d, dic){

  # https://stackoverflow.com/questions/38983179/do-call-a-function-in-r-without-loading-the-package
  getfun <- function(x) {
    if(length(grep("::", x)) > 0) {
      parts <- strsplit(x, "::")[[1]]
      getExportedValue(parts[1], parts[2])
    } else {
      x
    }
  }

  stats <- purrr::map(d, function(col){
    hdtype <- hdTypes::which_hdType(col)
    if(length(hdtype) != 0){
      do.call(getfun(paste0("hdTypes::",hdtype, "_get_stats")), list(col))
    }else{
      NA
    }
  })

  list(nrow = nrow(d), ncol = ncol(d), col_stats = stats)
}

#' @export
hdTable_stats <- function(f){
  f$stats
}


#' @export
hdTable_update_meta <- function(f, ...){
  fixed <- c("data", "dic", "hdTableType", "hdTableGroupType")
  args <- list(...)
  str(args)
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
  f$meta <- modifyList(f$meta, meta)
  f
}




#' @export
hdTable_labels <- function(f){
  labels <- f$dic$label
  names(labels) <- letterNames(f$stats$ncol)
  labels
}

#' @export
hdTable_ids <- function(f){
  f$dic$id
}







#' @export
hdTable_column <- function(f, column){
  idx <- NULL
  if(is.numeric(column)){
    idx <- column
  }else{
    if(is.character(column)){
      idx <- match(column, hdTable_labels(f))
      if(is.na(idx)){
        idx <- match(column, hdTable_ids(f))
      }
      if(column %in% letters){
        d <- hdTable_d(f)
        return(d[[column]])
      }
    }
  }
  if(is.null(idx)) stop("column not found")
  hdTable_d(f)[[idx]]
}


#'@export
hdTable_d <- function(f){
  purrr::map_df(f$data, as_baseType) %>%
    purrr::set_names(letterNames(nrow(f$dic)))
}

#'@export
hdTable_data <- function(f, labels = FALSE){
  data <- hdTable_d(f)
  if(labels){
    names(data) <- hdTable_dic(f)$label
  }else{
    names(data) <- hdTable_dic(f)$id
  }
  class(data) <- class(data)[class(data) != "hd_tbl"]
  data
}

#'@export
hdTable_hdTibble <- function(f){
  data <- f$data
  if(!"hd_tbl" %in% class(data)){
    class(data) <- c(class(data), "hd_tbl")
  }
  data
}

#' @export
hdTable_dic <- function(f, id_letters = FALSE, stats = FALSE){
  dic <- f$dic
  if(id_letters)
    dic$id_letters <- letterNames(nrow(dic))
  if(stats){
    col_stats <- hdTable_stats(f)$col_stats
    dic$stats <- unname(col_stats)
  }
  dic
}

#'@export
hdTable_hdTypes <- function(fr, named = FALSE){
  x <- fr$dic$hdType
  if(named) names(x) <- fr$dic$id
  x
}

#'@export
hdTable_hdTableType <- function(fr){
  as.character(fr$hdTableType)
}




#' #' @export
#' force_hdTypes <- function(df, hdTypes){
#'   df <- as.data.frame(df)
#'   if(ncol(df)!= length(hdTypes)) stop("number of df cols must be the same as col types length")
#'   for (i in seq_along(hdTypes)){
#'     if(hdTypes[i]=="Num"){df[,i]<- as.numeric(df[,i])}
#'     if(hdTypes[i]=="Yea"){df[,i]<- as.character(df[,i])}
#'     if(hdTypes[i]=="Cat"){df[,i]<- as.character(df[,i])}
#'     if(hdTypes[i]=="Txt"){df[,i]<- as.character(df[,i])}
#'     if(hdTypes[i]=="Img"){
#'       if(!isImgUrl(df[,i])) stop ("Not an image Url")
#'       df[,i]<- as.character(df[,i])
#'     }
#'     if(hdTypes[i]=="Dat"){df[,i]<- parseDatetime(df[,i],"Dat")}
#'     if(hdTypes[i]=="Hms"){df[,i]<- parseDatetime(df[,i],"Hms")}
#'     if(hdTypes[i]=="Dti"){df[,i]<- parseDatetime(df[,i],"Dti")}
#'     if(hdTypes[i]=="Glt"){df[,i]<- as.numeric(df[,i])}
#'     if(hdTypes[i]=="Gln"){df[,i]<- as.numeric(df[,i])}
#'     if(hdTypes[i]=="Gnm"){df[,i]<- as.character(df[,i])}
#'   }
#'   as_tibble(df)
#' }


