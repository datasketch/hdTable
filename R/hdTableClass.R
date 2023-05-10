
hdTableClass <- R6::R6Class(
  "hdTable",
  public = list(
    dic = NULL,
    hdTableType = NULL,
    name = NULL,
    slug = NULL,
    description = NULL,
    meta = NULL,
    hdTableTypeGroup = NULL,
    data = NULL,
    nrow = NULL,
    ncol = NULL,

    initialize = function(d, dic = NULL, hdTableType = NULL,
                          name = NULL, description = NULL,
                          slug = NULL, meta = NULL,
                          formats =  NULL,
                          nrow = NULL,
                          ncol = NULL) {

      name <- name %||% deparse(substitute(d))
      description <- description %||% ""
      slug <- slug %||% dstools::create_slug(name)
      formats <- unique(c('csv', 'json'), formats)

      if(is.null(dic)){
        dic <- create_dic(d, hdTableType = hdTableType)
      } else {
        dic$hdType <- dic$hdType %||% hdTableType_hdTypes(guess_hdTableType(d))
        dic$hdType <- as_hdType(dic$hdType)
        dic <- tibble::as_tibble(dic)
      }
      if(!is_hdTibble(d)){
        d <- hdTibble(d, dic)
      }

      self$name <- name
      self$description <- description
      self$slug <- slug
      self$meta <- meta

      self$stats <- calculate_hdTable_stats(d, dic)

      self$dic <- dic
      self$data <- d
      self$hdTableType <- hdTableType(paste0(dic$hdType, collapse = "-"))
      self$hdTableTypeGroup <- get_hdTableTypeGroup(hdTableType(dic$hdType))

      self$nrow <- nrow(self$data)
      self$ncol <- ncol(self$data)


    },

    d = function(){
      self$data |> setNames(self$dic$id)
    },
    meta = function(){

        base_info <- list(
          name = self$name,
          description = self$description,
          slug = self$slug,
          formats = self$formats,
          hdTableType = self$hdTableType,
          hdTableTypeGroup = self$hdTableTypeGroup,
          nrow = self$nrow,
          ncol = self$ncol,
        )
        stats <- self$stats()
        c(base_info, self$meta)
    },
    get_stats = function(){
      attr(self$vector, "stats")
    }
    write_csv = function(path = "", overwrite_dic = FALSE){
      file.path(path,paste0(self$slug,".csv"))
      readr::write_csv(self$data, save_path)
      dic_path <- file.path(path,paste0(self$slug,".dic.csv"))
      if(file.exists(dic_path) && !overwrite_dic ){
        stop("Cannot overwrite dic")
      }
      readr::write_csv(self$dic, dic_path)
    },
    write_json = function(path = "", overwrite_dic = FALSE){
      save_path <- file.path(path,paste0(self$slug,".json"))
      jsonlite::write_json(self$data, save_path, auto_unbox = TRUE)
    },
    write_meta_json = function(path = "", overwrite_dic = FALSE){
      save_path <- file.path(path,paste0(self$slug,".meta.json"))
      jsonlite::write_json(self$meta(), save_path, auto_unbox = TRUE)
    },
  )
)







