

random_id_vector <- function(len){
  purrr::map_chr(1:len, ~ dstools::random_name(n = 8))
}


col_ids_from_name <- function (x, sep = "_"){
  x <- gsub("[^[:alnum:]]", "_", x)
  x <- dstools::remove_accents(x)
  x <- tolower(x)
  x <- gsub("-+", "_", x)
  x <- gsub("[[:punct:]]+","_",x)
  x <- gsub("+[[:punct:]]$", "", x)
  x <- gsub("^-.", "", x)
  x


  # x <- gsub("[^[:alnum:]]", "-", x)
  # x <- dstools::remove_accents(tolower(x))
  # x <- gsub("-+", "-", x)
  # x <- gsub("^-.", "", x)
  # x

}



#' @export
letterNames <- function(n){
  if(n<27)
    return(letters[1:n])
  if(n<703){
    l2 <- expand(tibble(A=letters,B=letters),A,B) |>
      tidyr::unite("l",A,B,sep="") |> dplyr::pull(l)
    return(c(letters,l2)[1:n])
  }
  if(n < 18279){ # 26 + 676 + 17576 = 18278
    l2 <- expand(tibble(A=letters,B=letters),A,B) |>
      tidyr::unite("l",A,B,sep="") |>  dplyr::pull(l)
    l3 <- expand(tibble(A=letters,B=letters,C=letters),A,B,C) |>
      tidyr::unite("l",A,B,C,sep="") |>  dplyr::pull(l)
    return(c(letters,l2,l3)[1:n])
  }
  stop("Cannot handle data with more than 18279 columns")
}


loremNames <- function(ncol){
  lorem0 <- "lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor
  incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
  quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
  consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
  cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
  proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  lorem1 <- gsub("[[:punct:]|\n]", "", lorem0)
  lorem1 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", lorem1, perl=TRUE)
  lorem2 <- unique(firstup(strsplit(lorem1," ",fixed = TRUE)[[1]]))
  sample2(lorem2,ncol,replace = FALSE)
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

sample2 <- function(v, n,replace = TRUE, ...){
  if(length(v)==1) return(rep(v,5))
  sample(v,n,replace = replace, ...)
}



