
clean_names <- function(){

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



