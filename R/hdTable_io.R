

#' @export
hdTable_write <- function(hdtab, path = ""){
  if(!is_hdTable(x))
    stop("x is not a hdTable")
  hdtab$write(path)
}


#' @export
hdTable_read <- function(path, slug = NULL){

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
                       "hdTableType", "hdTableTypeGroup", "ncol", "nrow",
                       "credits")
  additional_meta <- meta_json[!names(meta_json) %in% standard_fields]

  d <- readr::read_csv(file.path(path, paste0(slug,".csv")),
                       col_types = readr::cols())
  dic <- readr::read_csv(file.path(path, paste0(slug,".dic.csv")),
                         col_types = readr::cols())
  dic <- update_dic(dic, d)
  names(d) <- dic$label
  # dic <- jsonlite::read_json(file.path(path, paste0(slug,".dic.json")))
  #
  # purrr::map(dic, tibble::as_tibble)
  #
  # x <- purrr::transpose(dic)
  # tibble::as_tibble(x)
  #
  # names(d) <- dic$label

  hdTable(d, dic = dic,
         name = meta_json$name, description = meta_json$description,
         slug = meta_json$slug,
         meta = additional_meta)

}





