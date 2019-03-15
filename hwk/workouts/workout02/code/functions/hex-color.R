library(stringr)

hex <- "#1234GF"

#'@title hex color checker
#'@description Checks whether a given string is a
#'valid color in hexidecimal format. The function
#'returns a boolean truth value for whether the
#'string is valid or not
#'@param hex (string)
#'@output y. boolean truth value

is_hex <- function(hex){
  if(is.character(hex) == FALSE | length(hex)!= 1){
    stop("invalid input; a string was expected")
  } else{
  return(str_detect(hex,"#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$"))
  }
}