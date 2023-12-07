
#' @export
clean_names <- function(x) {

  x[x == ""] <- "x"

  # Change special symbols
  x <- change_special_symbols(x)


  # Replace specific symbols with text equivalents
  replacements <- c(`%` = "percent", `#` = "num",
                    `'` = "", `´` = "", `"` = "", `\`` = "",
                    `€` = "euro", `$` = "currency")
  for (symbol in names(replacements)) {
    replacement <- replacements[[symbol]]
    x <- stringr::str_replace_all(x, stringr::fixed(symbol), replacement)
  }

  # Change one character of puntuation to x
  #x <- stringr::str_replace_all(x, "[[:punct:]]", "x")
  # x <- stringr::str_replace_all(x, "[[:punct:]+&&[^_]]+", "x")

  # Replace more than one character of punctuation for an underscore
  x <- stringr::str_replace_all(x, "[[:punct:]+&&[^_]]+", "_")
  #stringr::str_replace_all(names, "[[:punct:]]+", "_")

  # Remove unwanted characters and replace with underscores
  x <- gsub("[^[:alnum:] ]", "_", x)

  # Replace punctuation with underscores
  x <- gsub("[[:punct:^_]]+","_",x)

  # Replace spaces with underscores
  x <- gsub(" ", "_", x)

  # # Leading spaces or puntuation
  # x <- gsub("[ ]+","",x)

  # Remove leading spaces and punctuation
  #x <- stringr::str_replace(x, "^[[:punct:][:space:]]+", "")
  x <- stringr::str_replace(x, "^[[:punct:][:space:]&&[^_]]+", "")
  # Remove trailing spaces and punctuation
  #x <- stringr::str_replace(x, "[[:punct:][:space:]]+$", "")
  x <- stringr::str_replace(x, "[[:punct:][:space:]&&[^_]]+$", "")

  # Convert to ASCII to handle unicode characters
  x <- stringi::stri_trans_general(x, "Latin-ASCII")

  # Replace any leading numbers with "x"
  x <- ifelse(stringr::str_detect(x, "^[0-9]"), paste0("x", x), x)

  # Convert camel case to snake case
  x <- stringr::str_replace_all(x, "(?<=[a-z])(?=[A-Z])", "_")

  # Remove leading underscores
  x <- stringr::str_replace(x, "^[_]+", "")
  # Remove trailing underscores
  x <- stringr::str_replace(x, "[_]+$", "")


  x <- tolower(x)
  x[x == "_"] <- "x"
  x[x == ""] <- "x"
  # Ensure names are unique and not empty
  x <- make.names(x, unique = TRUE)
  x <- gsub("\\.", "_", x)


  tolower(x)

}


change_special_symbols <- function (string){
  accents <- "µ*"
  translation <- "mx"
  chartr(accents, translation, string)
}


