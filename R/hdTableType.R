
new_hdtable_type <- function(x = character()){
  vctrs::vec_assert(x, character())
  if(length(x)>1){
    hdtypes <- lapply(strsplit(x, "-", fixed = TRUE), hdtype)
    group <- get_hdtable_type_group(x)
  } else if (length(x) == 1){
    hdtypes <- hdtype(strsplit(x, "-", fixed = TRUE)[[1]])
    group <- get_hdtable_type_group(x)
  } else {
    hdtypes <- hdtype()
    group <- NULL
  }
  vctrs::new_vctr(x, hdtypes = hdtypes, group = group, class = "hdtable_type")
}

#' @title hdtable_type Vectors
#'
#' @description a grouped way of reading hdtable_types values
#'
#' @param hdtable_type_str a string value showing a grouped hdtable_types view
#'
#' @return a grouped view of given hdtable_types values
#'
#' @examples
#'
#' x <- c("Cat-Num-Cat")
#' fr <- hdtable_type(x)
#' get_hdtable_type_group(fr)

#'
#' @export
get_hdtable_type_group <- function(hdtable_type_str){

  if(is.null(hdtable_type_str)) return()

  ctps <- strsplit(hdtable_type_str,"-")
  f <- function(hdtypes){
    ct <- dplyr::count(tibble::tibble(hdtypes = hdtypes),hdtypes)
    ct$n[ct$n == 1] <- ""
    ctv <- tidyr::unite(ct,hdtype,hdtypes,n,sep="") %>% .[[1]] %>% sort()
    paste(ctv,collapse="-")
  }
  purrr::map_chr(ctps, f)
}



#' @title hdtable_type Vectors
#'
#' @description Reverses the effect of [get_hdtable_type_group()] and split every single hdtable_type from an object.
#'
#' @param hdtable_type_group a grouped hdtable_type object
#'
#' @return a string value showing all the hdtable_type values of an object
#'
#' @examples
#'
#' x <- c("Cat-Num-Cat")
#' fr <- hdtable_type(x)
#' grouped_fr <- get_hdtable_type_group(fr)
#' expand_frGroup(grouped_fr)
#'
#' @export
expand_hdtable_type_group <- function(hdtable_type_group){
  ft1 <- strsplit(hdtable_type_group,"-",fixed = TRUE)[[1]]
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


#' @title hdtable_type Vectors
#'
#' @description Creates or coerces objects of type "hdtable_type"
#'
#' @param x object to be created or coerced
#'
#' @return returns a hdtable_type value
#'
#' @examples
#'
#' hdtable_type("Cat")
#'
#' @export
hdtable_type <- function(x = character()) {

  if(is.null(x)) return()

  if(is_hdtable(x)){
    return(x$hdtable_type)
  }

  if(is_hdtype(x)){
    # x <- vctrs::vec_cast(x, character()) # Not working!
    x <- vctrs::vec_data(x)
    x <- paste(x, collapse = "-")
  }
  x <- vctrs::vec_cast(x, character())
  new_hdtable_type(x)
}





#' @title hdtable_type_group of an hdtable
#'
#' @description Gets the hdtable_type_group from an hdtable
#'
#' @param x hdtable
#'
#' @return returns a string
#'
#' @examples
#'
#' hdtable_type("Cat")
#'
#' @export

hdtable_type_group <- function(x){
  if(is_hdtable(x)){
    return(x$hdtable_type_group)
  }
  if(!is_hdtable_type(x)) stop("x must be a hdtable_type")
  attr(x, "group")
}




#' @title hdtable_type Vectors
#'
#' @description Creates or test for objects of type "hdtable_type"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hdtable_type or not.
#'
#' @examples
#'
#' value <- hdtable_type("Cat")
#' is_hdtable_type(value)
#'
#' @export
is_hdtable_type <- function(x) {
  inherits(x, "hdtable_type")
}


#' @title hdtable_type Vectors
#'
#' @description coerces its argument to a hdtable_type. It is an abbreviated form of hdtable_type.
#'
#' @param x object to be coerced
#'
#' @return attempts to coerce its argument to hdtable_type type
#'
#' @examples
#'
#' some_chr_value <- "Cat"
#' class(some_chr_value)
#'
#' some_frt_value <- as_hdtable_type(some_chr_value)
#' class(some_frt_value)
#'
#' @export
as_hdtable_type <- function(x) {
  vctrs::vec_cast(x, new_hdtable_type())
}





#' @title hdtable_type Vectors
#'
#' @description convert hdtable_types value(s) into hdtype
#'
#' @param x an available hdtable_type value
#'
#' @return an hdtype value
#'
#' @examples
#'
#' x <- hdtable_type("Cat")
#' class(x)
#'
#' x_hdt <- hdtable_type_hdtypes(x)
#' class(x_hdt)
#'
#' @export
hdtable_type_hdtypes <- function(x, chr = FALSE){
  if(!is_hdtable_type(x)) stop("x must be a hdtable_type")
  hdt <- attr(x, "hdtypes")
  if(chr) hdt <- as.character(hdt)
  hdt
}



#' @title hdtable_type Vectors
#'
#' @description convert hdtable_types value(s) into character
#'
#' @param x an available hdtable_type value, hdtable_type dataframe or hd_tbl dataframe
#'
#' @return a character value
#'
#' @examples
#'
#' x <- hdtable_type("Cat")
#' class(x)
#'
#' x_chr <- hdtable_type_str(x)
#' class(x_chr)
#'
#' @export
hdtable_type_str <- function(x){
  if(is_hdtable_type(x)){
    return(paste(vctrs::vec_data(hdtable_type_hdtypes(x)),collapse = "-"))
  }
  if("data.frame" %in% class(x) || "hd_tbl" %in% class(x)){
    return(paste0(purrr::map_chr(x, hdtype::which_hdtype), collapse = "-"))
  }
}





# Methods

## Format method

#' @export
format.hdtable_type <- function(x, ...) {
  sprintf(fmt = "%s", x)
}

#' @export
vec_ptype_abbr.hdtable_type <- function(x, ...) {
  "hdtable_type"
}

# Coercion

#' @method vec_ptype2 hdtable_type
#' @export
vec_ptype2.hdtable_type <- function(x, y, ...) UseMethod("vec_ptype2.hdtable_type", y)

#' @method vec_ptype2.hdtable_type default
#' @export
vec_ptype2.hdtable_type.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
# A hdtable_type combined with a hdtable_type returns a hdtable_type

#' @method vec_ptype2.hdtable_type hdtable_type
#' @export
vec_ptype2.hdtable_type.hdtable_type <- function(x, y, ...) new_hdtable_type()

# # hdtable_type and character return double

#' @method vec_ptype2.hdtable_type character
#' @export
vec_ptype2.hdtable_type.character <- function(x, y, ...) hdtable_type()

#' @method vec_ptype2.character hdtable_type
#' @export
vec_ptype2.character.hdtable_type <- function(x, y, ...) hdtable_type()

# Casting

#' @method vec_cast hdtable_type
#' @export
vec_cast.hdtable_type <- function(x, to, ...) UseMethod("vec_cast.hdtable_type")

#' @method vec_cast.hdtable_type default
#' @export
vec_cast.hdtable_type.default <- function(x, to, ...) vec_default_cast(x, to)
# Coerce hdtable_type to hdtable_type

#' @method vec_cast.hdtable_type hdtable_type
#' @export
vec_cast.hdtable_type.hdtable_type <- function(x, to, ...) x

#' @method vec_cast.hdtable_type character
#' @export
vec_cast.hdtable_type.character <- function(x, to, ...) hdtable_type(x)

#' @method vec_cast.character hdtable_type
#' @export
vec_cast.character.hdtable_type <- function(x, to, ...) vctrs::vec_data(x)





