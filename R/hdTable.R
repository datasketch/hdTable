
#' @title Create a hdtable data frame
#' @description Create a hdtable object from a data frame. The main value of a hdtable is its metadata. When creating it, hdtable will add to the data frame the following information:
#'
#' - data: original data frame data. When it is created, the hdtable will convert original variable R types onto homodatum ones (Num, Cat, Pct, etc. -see [available_hdtypes()])
#' - dic: A diccionary is created with three variable characteristics: id, label and hdtype
#' - hdtable_type: Shows all variable types based in homodatum schema
#' - group: A grouped view of hdtable_type
#' - name: Name for the hdtable data frame, setted on _name_ argument
#' - description: Description for the hdtable data frame, setted on _description_ argument
#' - slug: a custom slug can be added
#' - stats: Depending on the variable type given by homodatum, the hdtable will generate different kind of statistics: nrow, ncol, n_unique, n_na, pct_na, min, max
#' @param x A data frame
#' @param hdtable_type The type of hdtable to create
#' @param dic a custom variable dictionary can be added. [create_dic()] can help you with that.
#' @param name a custom name can be added
#' @param nam a custom description can be added
#' @param slug a custom slug can be added. If not, hdtable will try creating one.
#' @param meta Custom Metadata can be added
#'
#' @examples
#' hdtable(mtcars, hdtable_type = "Num", name = "MTCars")
#'
#' @return A hdtable object
#' @export
hdtable <- function(d,
                    dic = NULL,
                    hdtable_type = NULL,
                    name = NULL,
                    description = NULL,
                    slug = NULL,
                    d_path = NULL,
                    meta = NULL,
                    formats = NULL,
                    lazy = FALSE,
                    ...){

  if(is_hdtable(d)) return(d)

  if(is.null(d)) return()

  # Check if it is a file
  if(is.character(d)){
    if(fs::is_file(d)){
      file <- d
      if(is_large_data(file) || lazy){
        dimension <- file_dimension(file)
        d <- NULL
        d_path <- file
      }else {
        d <- vroom::vroom(file)
      }
    }
  }

  name <- name %||% deparse(substitute(d))
  meta <- c(meta, list(...))
  if(dstools::is.empty(meta)) meta <- NULL


  hdtableClass$new(d, dic = dic, hdtable_type = hdtable_type,
               name = name, description = description,
               d_path = d_path,
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
  fixed <- c("data", "dic", "hdtable_type", "hdtableGroupType")
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





#' #' @export
#' force_hdtypes <- function(df, dic){
#'   hdtibble <- purrr::map2(d, dic$hdtype, function(col, hdtype){
#'     do.call(hdtype, list(col))
#'   })
#'   tibble::as_tibble(hdtibble)
#' }


