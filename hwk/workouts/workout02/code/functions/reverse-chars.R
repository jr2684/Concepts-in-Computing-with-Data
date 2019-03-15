#'@title String reverser
#'@description This function takes a string as 
#'an input and reverses string
#'@param phrase. This is a string (string)
#'@output y. Reversed string given by phrase (string)
#'
reverse_chars <- function(phrase){
  letters <- c(strsplit(phrase, split = ""))
  reverse_letters <- letters[[1]][length(letters[[1]]):1]
  reverse_string <- paste(reverse_letters, collapse = "")
  return(reverse_string)
}
