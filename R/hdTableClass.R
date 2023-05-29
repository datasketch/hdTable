
hdtableClass <- R6::R6Class(
  "hdtable",
  public = list(
    dic = NULL,
    d_path = NULL,
    lazy = NULL,
    hdtable_type = NULL,
    name = NULL,
    slug = NULL,
    description = NULL,
    formats = NULL,
    meta = NULL,
    hdtable_type_group = NULL,
    dd = NULL,
    field_stats = NULL,
    nrow = NULL,
    ncol = NULL,
    magnitude = NULL,
    preview_max_nrow = NULL,
    preview_max_ncol = NULL,
    credits = NULL,

    initialize = function(d, dic = NULL, hdtable_type = NULL,
                          name = NULL, description = NULL,
                          slug = NULL, meta = NULL,
                          d_path = NULL,
                          formats =  NULL,
                          credits = NULL) {

      name <- name %||% deparse(substitute(d))
      description <- description %||% ""
      slug <- slug %||% dstools::create_slug(name)
      formats <- unique(c(c('csv', 'json'), formats))

      self$d_path <- d_path
      self$lazy <- FALSE
      if(!is.null(self$d_path)){
        self$lazy <- TRUE
      }

      original_names <- names(d)[names(d) != "rcd___id"]

      if(self$lazy && is.null(dic)){
        stop("If lazy need to provide dictionary")
      }

      if(is.null(dic)){
        dic <- create_dic(d, hdtable_type = hdtable_type)
      } else {
        dic$hdtype <- dic$hdtype %||% hdtable_type_hdtypes(guess_hdtable_type(d))
        dic$hdtype <- as_hdtype(dic$hdtype)
        if(is.null(dic$label)) dic$label <- dic$id
        if(!"fld___id" %in% names(dic)) dic$fld___id <- random_id_vector(nrow(dic))
        dic <- tibble::as_tibble(dic)

        if( !all(dic$id == col_ids_from_name(dic$id))){
          stop("Dictionary ids not clean to be uses as col names")
        }
        if(!all(original_names == dic$id)){
          stop("names(d) do not correspond to clean dic_ids")
        }

      }

      dd <- d
      if(!is_hdtibble(d)){
        dd <- hdtibble(d, dic)
      }
      if(! "rcd___id" %in% names(d)){
        if(!is.null(dd)){
          dd$rcd___id <- random_id_vector(nrow(d))
        }
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

      if(!is.null(dd)){
        names(dd) <- c(dic$label, "rcd___id")
      }
      self$dd <- dd

      self$hdtable_type <- hdtable_type(paste0(dic$hdtype, collapse = "-"))
      self$hdtable_type_group <- get_hdtable_type_group(hdtable_type(dic$hdtype))


      self$nrow <- nrow(self$dd) %||% file_nrow(self$d_path)
      self$ncol <- nrow(self$dic)  %||% file_ncol(self$d_path)
      self$magnitude <-   log10(self$nrow * self$ncol)

      self$preview_max_nrow <- 1000
      self$preview_max_ncol <- 10

      self$credits <- "Dataset hosted at http://datasketch.co"

    },
    dd_lazy_load = function(){
      if(is.null(self$dd) && !self$lazy) return()
      if(is.null(self$dd) && self$lazy){
        self$dd <- vroom::vroom(self$d_path, show_col_types = FALSE)
        if(is.null(self$dd$rcd___id)){
          self$dd$rcd___id <- random_id_vector(self$nrow)
        }
      }
    },
    df = function(){
      if(is.null(self$dd) && !self$lazy) return()
      self$dd_lazy_load()
      dout <- hdtibble_as_basetype(self$dd)
      dout |> dplyr::select(-rcd___id)
    },
    df_slug = function(){
      if(is.null(self$dd) && !self$lazy) return()
      self$dd_lazy_load()
      dout <- hdtibble_as_basetype(self$dd)
      dout <- dout |> dplyr::select(-rcd___id)
      dout |> purrr::set_names(self$dic$id)
    },
    df_slug_rcd = function(){
      if(is.null(self$dd) && !self$lazy) return()
      self$dd_lazy_load()
      dout <- hdtibble_as_basetype(self$dd)
      dout |> purrr::set_names(c(self$dic$id, "rcd___id"))
    },
    dic_no_fld = function(){
      self$dic |>
        dplyr::select(-fld___id, -format, -stats)
    },
    metadata = function(){
      base_info <- list(
        name = self$name,
        description = self$description,
        slug = self$slug,
        formats = self$formats,
        hdtable_type = self$hdtable_type,
        hdtable_type_group = self$hdtable_type_group,
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
      metadata$hdtable_type <- as.character(metadata$hdtable_type)
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
      self$write_meta_json(path)
      purrr::walk(self$formats, function(format){
        self[[paste0("write_", format)]](path)
      })

    },
    write_csv = function(path = ""){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".csv"))
      readr::write_csv(self$df(), save_path)
      dic_path <- file.path(path,paste0(self$slug,".dic.csv"))
      dic <- self$dic_no_fld()
      readr::write_csv(dic, dic_path)
    },
    write_json = function(path = ""){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path,  paste0(self$slug,".json"))
      dd <- self$df_slug_rcd()
      d <- hdtibble_as_basetype(dd)
      jsonlite::write_json(d, save_path, auto_unbox = TRUE,
                           pretty = TRUE, na = "null")
      # Save preview first 10 cols, 1000 rows
      # Only when data is bigger than the nrow and ncols of preview
      nc <- self$preview_max_ncol
      nr <- self$preview_max_nrow
      if(self$ncol > nc || self$nrow > nr){
        preview <- d |>
          dplyr::select(dplyr::any_of(1:nc)) |>
          dplyr::slice(1:nr)
        preview_path <- file.path(path,  paste0(self$slug,".preview.json"))
        jsonlite::write_json(preview, preview_path, auto_unbox = TRUE,
                             pretty = TRUE)
      }
      # Save dic.json
      dic_path <- file.path(path,paste0(self$slug,".dic.json"))
      dic <- self$dic
      dic$hdtype <- as.character(dic$hdtype)
      jsonlite::write_json(dic, dic_path, auto_unbox = TRUE, pretty = TRUE)
    },
    write_xlsx = function(path = ""){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".xlsx"))

      d <- self$df()
      dic <- self$dic_no_fld()
      dic$hdtype <- NULL

      info <- self$metadata()
      info$hdtable_type <- NULL
      info$hdtable_type_group <- NULL
      info <- unlist(info)
      info <- data.frame(label = names(info), value = info)
      names(info) <- c("", "")

      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Data")
      openxlsx::addWorksheet(wb, "Dictionary")
      openxlsx::addWorksheet(wb, "Info")

      openxlsx::writeDataTable(wb, 1, d)
      openxlsx::writeDataTable(wb, 2, dic)
      openxlsx::writeData(wb, 3, info)
      ## Not run:
      openxlsx::saveWorkbook(wb, file = save_path, overwrite = TRUE)

    }
  )
  ,
  active = list(
    data = function(value) {
      if (missing(value)){
        return(
          self$df_slug()
        )
      }
      ## TODO
      # hdt$data <- mtcars, # assigns mtcars to the data
      # need to validate and update dictionary
    }
  )
)







