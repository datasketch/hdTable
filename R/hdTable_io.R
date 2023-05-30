

#' @export
hdtable_write <- function(hdtab, path = ""){
  if(!is_hdtable(hdtab))
    stop("hdtab is not a hdtable")
  hdtab$write(path)
}


#' @export
hdtable_read <- function(path, slug = NULL, lazy = TRUE){

  metas <- list.files(path, pattern = "\\.meta\\.json")
  if(!is.null(slug)){
    meta <- paste0(slug, ".meta.json")
    if(!meta %in% metas){
      stop("Not metadata for slug found in path")
    }
  } else{
    meta <- metas[1]
  }

  meta_json <- jsonlite::read_json(file.path(path, meta), simplifyVector = TRUE)
  slug <- meta_json$slug

  standard_fields <- c("name", "description", "slug", "formats",
                       "hdtable_type", "hdtable_type_group", "ncol", "nrow",
                       "credits")
  additional_meta <- meta_json[!names(meta_json) %in% standard_fields]

  if(lazy){
    l <- read_csv_d_path_dic(path, slug)
    dic <- l$dic
    d <- l$d_path

  }else{
    l <- read_json_hdtibble_dic(path, slug)
    d <- l$hdtibble
    dic <- l$dic
  }

  hdtable(d, dic = dic,
         name = meta_json$name, description = meta_json$description,
         slug = meta_json$slug,
         meta = additional_meta,
         lazy = lazy,
         formats = meta_json$formats)

}


read_csv_d_path_dic <- function(path, slug){
  meta <- jsonlite::read_json(file.path(path, paste0(slug, ".meta.json")),
                              simplifyVector = TRUE)
  dic <- jsonlite::read_json(file.path(path, paste0(slug, ".dic.json")),
                             simplifyVector = TRUE)
  d_path <- paste0(file.path(path, slug),".csv")
  dic$format <- NULL
  dic$stats <- NULL

  list(d_path = d_path,
       dic = dic)

}




read_json_hdtibble_dic <- function(path, slug){
  meta <- jsonlite::read_json(file.path(path, paste0(slug, ".meta.json")),
                              simplifyVector = TRUE)
  d <- jsonlite::read_json(file.path(path, paste0(slug, ".json")),
                              simplifyVector = TRUE)
  rcd___id <- d$rcd___id
  dic <- jsonlite::read_json(file.path(path, paste0(slug, ".dic.json")),
                             simplifyVector = TRUE)
  format <- d$format
  stats <- d$stats
  dic$format <- NULL
  dic$format <- format
  dic$stats <- NULL
  dic$stats <- stats

  dic <- update_dic(dic,d)

  list(hdtibble = hdtibble(d, dic = dic),
       dic = dic)

}


