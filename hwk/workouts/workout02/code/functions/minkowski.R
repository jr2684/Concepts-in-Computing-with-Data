#' @title Minkowski distance
#' @description This function calculates the minkowski
#' distance. If no p value is given, then the function
#' calculate the 1-norm distance. If one wishes to 
#' calculate the infinity norm, then the input "max"
#' is required
#' @param x tuple of length n (vector)
#' @param x tuple of length n (vector)
#' @param p real valued number (numeric)
#' @output y calculated minkowski distance (numeric)
minkowski <- function(x,y,p = FALSE){
  if(p == FALSE){
    sum = 0
    for(i in 1:length(x)){
      sum = sum + (abs(x[i] - y[i]))
    }
    return(sum)
  }else{
  if(length(x) != length(y)){
    print("x and y have different lengths")
  }
  if(is.character(p) == TRUE){
    if(p =="max"){
      max_values = c()
      for(i in 1:length(x)){
        max_values <- c(max_values,(abs(x[i] - y[i])))
      }
      return(max(max_values))
    } 
    else{
      stop("invalid character value for p")
    }
  } else if(is.numeric(p)){
    if(p < 1){
      stop("p cannot be less than 1")
    } else{
    sum = 0
    for(i in 1:length(x)){
      sum = sum + (abs(x[i] - y[i]))^p
        }
    return((sum)^(1/p))
      }
    }
  }
}