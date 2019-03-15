#'@title vowel counter
#'@description This function takes a string as 
#'input and outputs the number of vowels present
#'in that string. 
#'@param y. input string (string)
#'@output z. numeric vector containing the
#'number of vowels (list)

count_vowels <- function(y){
  if(is.character(y) == FALSE){
    stop("invalid input, a string was expected")
  } else{
  x <- tolower(strsplit(y, "")[[1]])
  x <- table(x[x %in% letters])
  return(x[c("a", "e", "i", "o", "u")])
  }
}