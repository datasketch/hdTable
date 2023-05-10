
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
    stats = NULL,

    initialize = function(d, dic = NULL, hdTableType = NULL,
                          name = NULL, description = NULL,
                          slug = NULL, meta = NULL) {

      name <- name %||% deparse(substitute(d))
      description <- description %||% ""
      slug <- slug %||% dstools::create_slug(name)

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

    },
    get_stats = function(){
      attr(self$vector, "stats")
    }
  )
)


