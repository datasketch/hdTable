

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

  if(dstools::is_url(path)){
    if(is.null(slug)){
      json_path <- paste0(path, ".meta.json")
    }else{
      json_path <- paste0(file.path(path, slug), ".meta.json")
    }
    meta_list <- jsonlite::read_json(json_path,
                                     simplifyVector = TRUE)
  }else{
    metas <- list.files(path, pattern = "\\.meta\\.json")
    if(length(metas) > 0){
      if(!is.null(slug)){
        meta <- metas[grepl(slug, metas)]
      }else{
        meta <- metas[1]
      }
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

  }




  standard_fields <- c("name", "description", "slug", "formats",
                       "hdtable_type", "hdtable_type_group", "ncol", "nrow",
                       "credits")
  additional_meta <- meta_list[!names(meta_list) %in% standard_fields]

  if(lazy){
    l <- read_csv_d_path_dic(path, slug = meta_list$slug)
    dic <- l$dic
    d <- l$d_path

  }else{
    l <- read_json_hdtibble_dic(path, slug = meta_list$slug)
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

  ### READ DICTIONARY FROM JSON? OR CSV?
  # Warning: dic doesn't exist for large files
  # dic_path <- file.path(path, paste0(slug, ".dic.json"))
  # if(dstools::is_url(dic_path) || file.exists(dic_path)){
  #   dic <- jsonlite::read_json(file.path(path, paste0(slug, ".dic.json")),
  #                              simplifyVector = TRUE)
  # }

  dic_path <- file.path(path, paste0(slug, ".dic.csv"))
  if(dstools::is_url(dic_path) || file.exists(dic_path)){
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

  json_path <- file.path(path, paste0(slug, ".meta.json"))

  if(dstools::is_url(json_path) || file.exists(json_path)){
    meta <- jsonlite::read_json(file.path(path, paste0(slug, ".meta.json")),
                                simplifyVector = TRUE)
  } else{
    meta <- list()
  }

  csv_path_slug <- file.path(path, paste0(slug, ".csv"))
  if(dstools::is_url(json_path) || file.exists(csv_path_slug)){
    d <- vroom::vroom(csv_path_slug,
                      show_col_types = FALSE)
  }

  if("rcd___id" %in% names(d)){
    rcd___id <- d$rcd___id
  }

  ### READ DICTIONARY FROM JSON? OR CSV?
  # dic_path <- file.path(path, paste0(slug, ".dic.json"))
  # if(file.exists(dic_path) || dstools::is_url(dic_path)){
  #   dic0 <- jsonlite::read_json(dic_path,
  #                               simplifyDataFrame = FALSE)
  #   dic <- purrr::map(dic0, `[`, c("id", "label", 'hdtype')) |> dplyr::bind_rows()
  #   format <- purrr::map(dic0, `[`, "format") |> setNames(dic$id)
  #   dic$format <- format
  #   stats <- purrr::map(dic0, `[`, "stats") |> setNames(dic$id)
  #   dic$stats <- stats
  #   dic
  # }
  dic_path <- file.path(path, paste0(slug, ".dic.csv"))
  if(file.exists(dic_path) || dstools::is_url(dic_path)){
    dic <- vroom::vroom(dic_path,
                        show_col_types = FALSE)
    dic$format <- NULL
    dic$stats <- NULL
    # if("stats" %in% names(dic)){
    #   dic <- dic |>
    #     dplyr::mutate(
    #       format = purrr::map(format, jsonlite::fromJSON, null = "null") |> setNames(id),
    #       stats = purrr::map(stats, jsonlite::fromJSON, null = "null") |> setNames(id)
    #     )
    # }
  }



  if(!"stats" %in% names(dic)){
    dic <- update_dic(dic,d)
  }

  list(hdtibble = hdtibble(d, dic = dic),
       dic = dic)

}


