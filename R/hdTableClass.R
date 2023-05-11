
hdTableClass <- R6::R6Class(
  "hdTable",
  public = list(
    dic = NULL,
    hdTableType = NULL,
    name = NULL,
    slug = NULL,
    description = NULL,
    formats = NULL,
    meta = NULL,
    hdTableTypeGroup = NULL,
    data = NULL,
    field_stats = NULL,
    nrow = NULL,
    ncol = NULL,
    credits = NULL,

    initialize = function(d, dic = NULL, hdTableType = NULL,
                          name = NULL, description = NULL,
                          slug = NULL, meta = NULL,
                          formats =  NULL,
                          nrow = NULL,
                          ncol = NULL,
                          credits = NULL) {

      name <- name %||% deparse(substitute(d))
      description <- description %||% ""
      slug <- slug %||% dstools::create_slug(name)
      formats <- unique(c(c('csv', 'json'), formats))

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
      if(!all(formats %in% self$available_write_formats())){
        stop("Cannot write in the format specified. Formats supported are: ",
             paste(self$available_write_formats(), collpase = ", "))
      }
      self$formats <- formats
      self$meta <- meta

      self$dic <- dic
      self$data <- d
      self$hdTableType <- hdTableType(paste0(dic$hdType, collapse = "-"))
      self$hdTableTypeGroup <- get_hdTableTypeGroup(hdTableType(dic$hdType))

      self$nrow <- nrow(self$data)
      self$ncol <- ncol(self$data)

      self$credits <- "Dataset hosted at http://datasketch.co"


    },

    d = function(){
      d <- purrr::map_df(self$data, as_baseType)
      d |> setNames(self$dic$id)
    },
    tibble = function(){
      self$data |> setNames(self$dic$id)
    },
    metadata = function(){
      base_info <- list(
        name = self$name,
        description = self$description,
        slug = self$slug,
        formats = self$formats,
        hdTableType = self$hdTableType,
        hdTableTypeGroup = self$hdTableTypeGroup,
        nrow = self$nrow,
        ncol = self$ncol,
        credits = self$credits
      )
      #stats <- self$field_stats
      c(base_info, self$meta)
    },
    write_meta_json = function(path = "", overwrite_dic = FALSE){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path,paste0(self$slug,".meta.json"))
      metadata <- self$metadata()
      metadata$hdTableType <- as.character(metadata$hdTableType)
      jsonlite::write_json(metadata, save_path,
                           auto_unbox = TRUE, pretty = TRUE)
    },
    available_write_formats = function(){
      nms <- names(self)
      # methods <- purrr::map(nms, ~ class(self[[.]])) |>
      #  purrr::set_names(nms)
      nms <- nms[nms != "write_meta_json"]
      nms <- nms[grepl("^write_", nms)]
      gsub("write_","", nms)
    },
    write = function(path = ""){
      message(path)
      self$write_meta_json(path)
      purrr::walk(self$formats, function(format){
        self[[paste0("write_", format)]](path)
      })

    },

    write_csv = function(path = ""){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".csv"))
      readr::write_csv(self$d(), save_path)
      dic_path <- file.path(path,paste0(self$slug,".dic.csv"))
      dic <- self$dic
      dic$format <- NULL
      dic$stats <- NULL
      readr::write_csv(dic, dic_path)
    },
    write_json = function(path = ""){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path,  paste0(self$slug,".json"))
      jsonlite::write_json(self$d(), save_path, auto_unbox = TRUE)
      dic_path <- file.path(path,paste0(self$slug,".dic.json"))
      dic <- self$dic
      dic$hdType <- as.character(dic$hdType)
      jsonlite::write_json(dic, dic_path, auto_unbox = TRUE, pretty = TRUE)
    },
    write_xlsx = function(path = ""){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".xlsx"))

      d <- self$d()
      dic <- self$dic
      dic$hdType <- NULL
      dic$format <- NULL
      dic$stats <- NULL

      info <- self$metadata()
      info$hdTableType <- NULL
      info$hdTableTypeGroup <- NULL
      info <- unlist(info)
      info <- data.frame(label = names(info), value = info)
      names(info) <- c("", "")

      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Data")
      openxlsx::addWorksheet(wb, "Dictionary")
      openxlsx::addWorksheet(wb, "Info")

      str(dic)

      openxlsx::writeDataTable(wb, 1, d)
      openxlsx::writeDataTable(wb, 2, dic)
      openxlsx::writeData(wb, 3, info)
      ## Not run:
      openxlsx::saveWorkbook(wb, file = save_path, overwrite = TRUE)

    }

  )
)







