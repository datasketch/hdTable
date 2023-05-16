
new_hdtableType <- function(x = character()){
  vctrs::vec_assert(x, character())
  if(length(x)>1){
    hdtypes <- lapply(strsplit(x, "-", fixed = TRUE), hdtype)
    group <- get_hdtableTypeGroup(x)
  } else if (length(x) == 1){
    hdtypes <- hdtype(strsplit(x, "-", fixed = TRUE)[[1]])
    group <- get_hdtableTypeGroup(x)
  } else {
    hdtypes <- hdtype()
    group <- NULL
  }
  vctrs::new_vctr(x, hdtypes = hdtypes, group = group, class = "hdtableType")
}

#' @title hdtableType Vectors
#'
#' @description a grouped way of reading hdtableTypes values
#'
#' @param hdtableType_str a string value showing a grouped hdtableTypes view
#'
#' @return a grouped view of given hdtableTypes values
#'
#' @examples
#'
#' x <- c("Cat-Num-Cat")
#' fr <- hdtableType(x)
#' get_hdtableTypeGroup(fr)

#'
#' @export
get_hdtableTypeGroup <- function(hdtableType_str){

  if(is.null(hdtableType_str)) return()

  ctps <- strsplit(hdtableType_str,"-")
  f <- function(hdtypes){
    ct <- dplyr::count(tibble::tibble(hdtypes = hdtypes),hdtypes)
    ct$n[ct$n == 1] <- ""
    ctv <- tidyr::unite(ct,hdtype,hdtypes,n,sep="") %>% .[[1]] %>% sort()
    paste(ctv,collapse="-")
  }
  purrr::map_chr(ctps, f)
}



#' @title hdtableType Vectors
#'
#' @description Reverses the effect of [get_hdtableTypeGroup()] and split every single hdtableType from an object.
#'
#' @param hdtableTypeGroup a grouped hdtableType object
#'
#' @return a string value showing all the hdtableType values of an object
#'
#' @examples
#'
#' x <- c("Cat-Num-Cat")
#' fr <- hdtableType(x)
#' grouped_fr <- get_hdtableTypeGroup(fr)
#' expand_frGroup(grouped_fr)
#'
#' @export
expand_hdtableTypeGroup <- function(hdtableTypeGroup){
  ft1 <- strsplit(hdtableTypeGroup,"-",fixed = TRUE)[[1]]
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


#' @title hdtableType Vectors
#'
#' @description Creates or coerces objects of type "hdtableType"
#'
#' @param x object to be created or coerced
#'
#' @return returns a hdtableType value
#'
#' @examples
#'
#' hdtableType("Cat")
#'
#' @export
hdtableType <- function(x = character()) {
  if(is.null(x)) return()
  if(is_hdtype(x)){
    # x <- vctrs::vec_cast(x, character()) # Not working!
    x <- vctrs::vec_data(x)
    x <- paste(x, collapse = "-")
  }
  x <- vctrs::vec_cast(x, character())
  new_hdtableType(x)
}



#' @title hdtableType Vectors
#'
#' @description Creates or test for objects of type "hdtableType"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hdtableType or not.
#'
#' @examples
#'
#' value <- hdtableType("Cat")
#' is_hdtableType(value)
#'
#' @export
is_hdtableType <- function(x) {
  inherits(x, "hdtableType")
}


#' @title hdtableType Vectors
#'
#' @description coerces its argument to a hdtableType. It is an abbreviated form of hdtableType.
#'
#' @param x object to be coerced
#'
#' @return attempts to coerce its argument to hdtableType type
#'
#' @examples
#'
#' some_chr_value <- "Cat"
#' class(some_chr_value)
#'
#' some_frt_value <- as_hdtableType(some_chr_value)
#' class(some_frt_value)
#'
#' @export
as_hdtableType <- function(x) {
  vctrs::vec_cast(x, new_hdtableType())
}




# hdtableType_group <- function(x){
#   if(!is_hdtableType(x)) stop("x must be a hdtableType")
#   attr(x, "group")
# }



#' @title hdtableType Vectors
#'
#' @description convert hdtableTypes value(s) into hdtype
#'
#' @param x an available hdtableType value
#'
#' @return an hdtype value
#'
#' @examples
#'
#' x <- hdtableType("Cat")
#' class(x)
#'
#' x_hdt <- hdtableType_hdtypes(x)
#' class(x_hdt)
#'
#' @export
hdtableType_hdtypes <- function(x, chr = FALSE){
  if(!is_hdtableType(x)) stop("x must be a hdtableType")
  hdt <- attr(x, "hdtypes")
  if(chr) hdt <- as.character(hdt)
  hdt
}



#' @title hdtableType Vectors
#'
#' @description convert hdtableTypes value(s) into character
#'
#' @param x an available hdtableType value, hdtableType dataframe or hd_tbl dataframe
#'
#' @return a character value
#'
#' @examples
#'
#' x <- hdtableType("Cat")
#' class(x)
#'
#' x_chr <- hdtableType_str(x)
#' class(x_chr)
#'
#' @export
hdtableType_str <- function(x){
  if(is_hdtableType(x)){
    return(paste(vctrs::vec_data(hdtableType_hdtypes(x)),collapse = "-"))
  }
  if("data.frame" %in% class(x) || "hd_tbl" %in% class(x)){
    return(paste0(purrr::map_chr(x, hdtype::which_hdtype), collapse = "-"))
  }
}





# Methods

## Format method

#' @export
format.hdtableType <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hdtableType <- function(x, ...) {
  "hdtableType"
}

# Coercion

#' @method vec_ptype2 hdtableType
#' @export
vec_ptype2.hdtableType <- function(x, y, ...) UseMethod("vec_ptype2.hdtableType", y)

#' @method vec_ptype2.hdtableType default
#' @export
vec_ptype2.hdtableType.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A hdtableType combined with a hdtableType returns a hdtableType

#' @method vec_ptype2.hdtableType hdtableType
#' @export
vec_ptype2.hdtableType.hdtableType <- function(x, y, ...) new_hdtableType()

# # hdtableType and character return double

#' @method vec_ptype2.hdtableType character
#' @export
vec_ptype2.hdtableType.character <- function(x, y, ...) hdtableType()

#' @method vec_ptype2.character hdtableType
#' @export
vec_ptype2.character.hdtableType <- function(x, y, ...) hdtableType()

# Casting

#' @method vec_cast hdtableType
#' @export
vec_cast.hdtableType <- function(x, to, ...) UseMethod("vec_cast.hdtableType")

#' @method vec_cast.hdtableType default
#' @export
vec_cast.hdtableType.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce hdtableType to hdtableType

#' @method vec_cast.hdtableType hdtableType
#' @export
vec_cast.hdtableType.hdtableType <- function(x, to, ...) x

#' @method vec_cast.hdtableType character
#' @export
vec_cast.hdtableType.character <- function(x, to, ...) hdtableType(x)

#' @method vec_cast.character hdtableType
#' @export
vec_cast.character.hdtableType <- function(x, to, ...) vctrs::vec_data(x)





