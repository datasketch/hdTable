

#' @export
hdtable_write <- function(hdtab, path = ""){
  if(!is_hdtable(hdtab))
    stop("hdtab is not a hdtable")
  hdtab$write(path)
}


#' @export
hdtable_read <- function(path, slug = NULL, lazy = TRUE){

  meta_list <- list(
    slug = slug,
    name = slug,
    description = NULL
  )

  metas <- list.files(path, pattern = "\\.meta\\.json")

  if(length(metas) > 0){
    meta <- metas[1]
    meta_list <- jsonlite::read_json(file.path(path, meta),
                                     simplifyVector = TRUE)
  } else{
    if(!is.null(slug)){
      if(file.exists(file.path(path, slug))){
        meta_list <- jsonlite::read_json(file.path(path, slug),
                                         simplifyVector = TRUE)
      }
    } else {
      csv_files <- list.files(path, pattern = "\\.csv")
      meta_list$slug <- unique(gsub("\\..*$", "", csv_files))[1]
      meta_list$name <- meta_list$slug
    }
  }


  standard_fields <- c("name", "description", "slug", "formats",
                       "hdtable_type", "hdtable_type_group", "ncol", "nrow",
                       "credits")
  additional_meta <- meta_list[!names(meta_list) %in% standard_fields]

  if(lazy){
    l <- read_csv_d_path_dic(path, meta_list$slug)
    dic <- l$dic
    d <- l$d_path

  }else{
    l <- read_json_hdtibble_dic(path, meta_list$slug)
    d <- l$hdtibble
    dic <- l$dic
  }

  hdtable(d, dic = dic,
          name = meta_list$name, description = meta_list$description,
          slug = meta_list$slug,
          meta = additional_meta,
          lazy = lazy,
          formats = meta_list$formats)

}


read_csv_d_path_dic <- function(path, slug){

  if(file.exists(file.path(path, paste0(slug, ".dic.json")))){
    dic <- jsonlite::read_json(file.path(path, paste0(slug, ".dic.json")),
                               simplifyVector = TRUE)

  }
  if(file.exists(file.path(path, paste0(slug, ".dic.csv")))){
    dic <- vroom::vroom(file.path(path, paste0(slug, ".dic.csv")),
                        show_col_types = FALSE)

  }

  d_path <- paste0(file.path(path, slug),".csv")
  dic$format <- NULL
  dic$stats <- NULL

  list(d_path = d_path,
       dic = dic)

}




read_json_hdtibble_dic <- function(path, slug){

  if(file.exists(file.path(path, paste0(slug, ".meta.json")))){
    meta <- jsonlite::read_json(file.path(path, paste0(slug, ".meta.json")),
                                simplifyVector = TRUE)
  } else{
    meta <- list()
  }

  if(file.exists(file.path(path, paste0(slug, ".json")))){
    d <- jsonlite::read_json(file.path(path, paste0(slug, ".json")),
                             simplifyVector = TRUE)
  }else if(file.exists(file.path(path, paste0(slug, ".dic.csv")))){
    d <- vroom::vroom(file.path(path, paste0(slug, ".csv")),
                      show_col_types = FALSE)
  }

  rcd___id <- d$rcd___id

  if(file.exists(file.path(path, paste0(slug, ".dic.json")))){
    dic <- jsonlite::read_json(file.path(path, paste0(slug, ".dic.json")),
                               simplifyVector = TRUE)
  } else if(file.exists(file.path(path, paste0(slug, ".dic.csv")))){
    dic <- vroom::vroom(file.path(path, paste0(slug, ".dic.csv")),
                        show_col_types = FALSE)
    dic <- dic |>
      dplyr::mutate(
        format = purrr::map(format, jsonlite::fromJSON),
        stats = purrr::map(stats, jsonlite::fromJSON)
      )

  }

  dic <- update_dic(dic,d)

  list(hdtibble = hdtibble(d, dic = dic),
       dic = dic)

}


