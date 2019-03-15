states <- rownames(USArrests)
# to determine the number of letters in each state's name
num_chars <- nchar(states)

#lengths of state names
state_lengths <- table(num_chars)
barplot(table(num_chars))

# pasting strings
paste(head(states), '=', nchar(head(states)))
paste(head(states), collapse = '')

#substrings
#first three letters
substr(states,1,3)
#last three letters
substr(states,nchar(states) - 2, nchar(states))

#first letter + last three letters
paste(substr(states,1,1),substr(states,nchar(states) - 2, nchar(states)),sep = '')

#challenge
states_list <- list()
for(i in type.convert(names(state_lengths),"integer")){
  states_list[paste(i,"chars",sep = '-')] <-  paste(states[num_chars == i], collapse = ", ")
}
  
#converting from fahrenheit degrees
to_celsius <- function(x = 1) {
  (x - 32) * (5/9)
}

to_kelvin <- function(x = 1) {
  (x + 459.67) * (5/9)
}

to_reaumur <- function(x = 1) {
  (x - 32) * (4/9)
}

to_rankine <- function(x = 1) {
  x + 459.67
}

temp_convert <- function(x = 1, to = "celsius") {
  switch(casefold(to),
         "celsius" = to_celsius(x),
         "kelvin" = to_kelvin(x),
         "reaumur" = to_reaumur(x),
         "rankine" = to_rankine(x))
}

# names of files
#1
b = c()
for(i in 1:10){
  b <- c(b,paste0("file",i,".csv"))
}
print(b)
#2
paste0("file",1:10,".csv")
#3
paste("file",1:10,".csv",sep = '')


#using the cat() function
# name of output file
outfile <- "output.txt"

# writing to 'outfile.txt'
cat("This is the first line", file = outfile)
# insert new line
cat("\n", file = outfile, append = TRUE)
cat("A 2nd line", file = "output.txt", append = TRUE)
# insert 2 new lines
cat("\n\n", file = outfile, append = TRUE)
cat("\nThe quick brown fox jumps over the lazy dog\n",
    file = outfile, append = TRUE)

#valid color names
is_color = function(x){
  if(x %in% colors()){
    return(TRUE)
  } else{
    return(FALSE)
  }
}
# scatterplot
is_color <- function(color){
  if(color %in% colors()){
    plot(runif(50,-1,1),runif(50,-1,1))
  }
  else
    stop(paste("invalid color",color, sep = " "))
}