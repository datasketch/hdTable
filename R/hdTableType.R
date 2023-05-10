
new_hdTableType <- function(x = character()){
  vctrs::vec_assert(x, character())
  if(length(x)>1){
    hdTypes <- lapply(strsplit(x, "-", fixed = TRUE), hdType)
    group <- get_hdTableTypeGroup(x)
  } else if (length(x) == 1){
    hdTypes <- hdType(strsplit(x, "-", fixed = TRUE)[[1]])
    group <- get_hdTableTypeGroup(x)
  } else {
    hdTypes <- hdType()
    group <- NULL
  }
  vctrs::new_vctr(x, hdTypes = hdTypes, group = group, class = "hdTableType")
}

#' @title hdTableType Vectors
#'
#' @description a grouped way of reading hdTableTypes values
#'
#' @param hdTableType_str a string value showing a grouped hdTableTypes view
#'
#' @return a grouped view of given hdTableTypes values
#'
#' @examples
#'
#' x <- c("Cat-Num-Cat")
#' fr <- hdTableType(x)
#' get_hdTableTypeGroup(fr)

#'
#' @export
get_hdTableTypeGroup <- function(hdTableType_str){
  ctps <- strsplit(hdTableType_str,"-")
  f <- function(hdtypes){
    ct <- dplyr::count(tibble::tibble(hdtypes = hdtypes),hdtypes)
    ct$n[ct$n == 1] <- ""
    ctv <- tidyr::unite(ct,hdtype,hdtypes,n,sep="") %>% .[[1]] %>% sort()
    paste(ctv,collapse="-")
  }
  purrr::map_chr(ctps, f)
}



#' @title hdTableType Vectors
#'
#' @description Reverses the effect of [get_hdTableTypeGroup()] and split every single hdTableType from an object.
#'
#' @param hdTableTypeGroup a grouped hdTableType object
#'
#' @return a string value showing all the hdTableType values of an object
#'
#' @examples
#'
#' x <- c("Cat-Num-Cat")
#' fr <- hdTableType(x)
#' grouped_fr <- get_hdTableTypeGroup(fr)
#' expand_frGroup(grouped_fr)
#'
#' @export
expand_hdTableTypeGroup <- function(hdTableTypeGroup){
  ft1 <- strsplit(hdTableTypeGroup,"-",fixed = TRUE)[[1]]
  cts <- substring(ft1,1,3)
  reps <- substring(ft1,4)
  purrr::flatten_chr(purrr::map2(cts,reps,function(x,y){
    # if(y == "P"){ # Are we still doing NumP things?
    #   return(rep(x,sample(2:6,1)))
    # }
    if(y == "") y = 1
    rep(x,as.numeric(y))
  }))
}


#' @title hdTableType Vectors
#'
#' @description Creates or coerces objects of type "hdTableType"
#'
#' @param x object to be created or coerced
#'
#' @return returns a hdTableType value
#'
#' @examples
#'
#' hdTableType("Cat")
#'
#' @export
hdTableType <- function(x = character()) {
  if(is_hdType(x)){
    # x <- vctrs::vec_cast(x, character()) # Not working!
    x <- vctrs::vec_data(x)
    x <- paste(x, collapse = "-")
  }
  x <- vctrs::vec_cast(x, character())
  new_hdTableType(x)
}



#' @title hdTableType Vectors
#'
#' @description Creates or test for objects of type "hdTableType"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hdTableType or not.
#'
#' @examples
#'
#' value <- hdTableType("Cat")
#' is_hdTableType(value)
#'
#' @export
is_hdTableType <- function(x) {
  inherits(x, "hdTableType")
}


#' @title hdTableType Vectors
#'
#' @description coerces its argument to a hdTableType. It is an abbreviated form of hdTableType.
#'
#' @param x object to be coerced
#'
#' @return attempts to coerce its argument to hdTableType type
#'
#' @examples
#'
#' some_chr_value <- "Cat"
#' class(some_chr_value)
#'
#' some_frt_value <- as_hdTableType(some_chr_value)
#' class(some_frt_value)
#'
#' @export
as_hdTableType <- function(x) {
  vctrs::vec_cast(x, new_hdTableType())
}




# hdTableType_group <- function(x){
#   if(!is_hdTableType(x)) stop("x must be a hdTableType")
#   attr(x, "group")
# }



#' @title hdTableType Vectors
#'
#' @description convert hdTableTypes value(s) into hdType
#'
#' @param x an available hdTableType value
#'
#' @return an hdType value
#'
#' @examples
#'
#' x <- hdTableType("Cat")
#' class(x)
#'
#' x_hdt <- hdTableType_hdTypes(x)
#' class(x_hdt)
#'
#' @export
hdTableType_hdTypes <- function(x, chr = FALSE){
  if(!is_hdTableType(x)) stop("x must be a hdTableType")
  hdt <- attr(x, "hdTypes")
  if(chr) hdt <- as.character(hdt)
  hdt
}



#' @title hdTableType Vectors
#'
#' @description convert hdTableTypes value(s) into character
#'
#' @param x an available hdTableType value, hdTableType dataframe or hd_tbl dataframe
#'
#' @return a character value
#'
#' @examples
#'
#' x <- hdTableType("Cat")
#' class(x)
#'
#' x_chr <- hdTableType_str(x)
#' class(x_chr)
#'
#' @export
hdTableType_str <- function(x){
  if(is_hdTableType(x)){
    return(paste(vctrs::vec_data(hdTableType_hdTypes(x)),collapse = "-"))
  }
  if("data.frame" %in% class(x) || "hd_tbl" %in% class(x)){
    return(paste0(purrr::map_chr(x, hdTypes::which_hdType), collapse = "-"))
  }
}





# Methods

## Format method

#' @export
format.hdTableType <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hdTableType <- function(x, ...) {
  "hdTableType"
}

# Coercion

#' @method vec_ptype2 hdTableType
#' @export
vec_ptype2.hdTableType <- function(x, y, ...) UseMethod("vec_ptype2.hdTableType", y)

#' @method vec_ptype2.hdTableType default
#' @export
vec_ptype2.hdTableType.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A hdTableType combined with a hdTableType returns a hdTableType

#' @method vec_ptype2.hdTableType hdTableType
#' @export
vec_ptype2.hdTableType.hdTableType <- function(x, y, ...) new_hdTableType()

# # hdTableType and character return double

#' @method vec_ptype2.hdTableType character
#' @export
vec_ptype2.hdTableType.character <- function(x, y, ...) hdTableType()

#' @method vec_ptype2.character hdTableType
#' @export
vec_ptype2.character.hdTableType <- function(x, y, ...) hdTableType()

# Casting

#' @method vec_cast hdTableType
#' @export
vec_cast.hdTableType <- function(x, to, ...) UseMethod("vec_cast.hdTableType")

#' @method vec_cast.hdTableType default
#' @export
vec_cast.hdTableType.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce hdTableType to hdTableType

#' @method vec_cast.hdTableType hdTableType
#' @export
vec_cast.hdTableType.hdTableType <- function(x, to, ...) x

#' @method vec_cast.hdTableType character
#' @export
vec_cast.hdTableType.character <- function(x, to, ...) hdTableType(x)

#' @method vec_cast.character hdTableType
#' @export
vec_cast.character.hdTableType <- function(x, to, ...) vctrs::vec_data(x)





