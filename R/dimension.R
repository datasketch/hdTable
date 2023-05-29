

is_large_data <- function(file){
  file_size(file) > 50e6 || file_dimension(file) > 5
}



file_dimension <- function(file){
  if(file_ext(file) %in% c("csv", "tsv")){
    nrow <- file_nrow(file)
    ncol <- file_ncol(file)
  }
  if(file_ext(file) == "json"){
    if(is_json_stream(file)){
      nrow <- file_nrow(file)
      line2 <- vroom::vroom_lines(file, n_max = 2)
      con <- textConnection(line2)
      json <- jsonlite::flatten(jsonlite::stream_in(con, verbose = FALSE))
      ncol <- ncol(json)
    } else{
      d <- jsonlite::fromJSON(file, simplifyDataFrame = TRUE)
      d <- jsonlite::flatten(d)
      nrow <- nrow(d)
      ncol <- ncol(d)
    }
  }
  log10(nrow * ncol)
}


file_nrow <- function(file){
  if(is.null(file)) return()
  length(vroom::vroom_lines(file, altrep = TRUE, progress = FALSE)) - 1L
}

file_ncol <- function(file){
  if(is.null(file)) return()
  ncol(vroom::vroom(file, n_max = 1, show_col_types = FALSE))
}


is_csv <- function(x){
  file_ext(x) == "csv"
}

is_json_stream <- function(x){
  jsonlite::validate(vroom::vroom_lines(x, n_max = 1))
}


