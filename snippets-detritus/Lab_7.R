#' @title area of rectangle
#' @description calculates the area of a rectangle
#' @param len length of the rectangle (numeric)
#' @param wid width of the rectangle (numeric)
#' @return computed area
rect_area <- function(len = 1, wid = 1) {
  if (len < 0) {
    stop("len must be positive")
  }
  if (wid < 0) {
    stop("wid must be positive")
  }
  area <- len * wid
  return(area)
}

#' @title simple mathematical function
#' @description simple quadratic function to test ones function writing ability
#' @param x real valued number (numeric)
#' @return the square of the param x
f <- function(x){
  return(x^2)
}

#' @title simple mathematical function
#' @description simple linear function to test ones function writing ability
#' @param x real valued number (numeric)
#' @return 3x+5

g <-  function(x){
  return(2*x + 5)
}


#' @title function composition 2
#' @description testing the composition of two functions
#' @param x function g evaluated at x (numeric)
#' @return f of g
fog <- function(x){
  return(f(g(x)))
}

#' @title function composition 1
#' @description testing a composite function
#' @param x function f evaluated at x (numeric)
#' @return g of f
gof <- function(x){
  return(g(f(x)))
}

#' @title pythagoras' theorem
#' @description the most famous geometric function in the world
#' @param a side of the right triangle (numeric)
#' @param b optional parameter. side of the right triangle (numeric)
#' @return c the calculated hypotenuse of the right triangle
 
pythagoras <- function(a, b = FALSE){
  if(b == FALSE){
    return(sqrt(2*a^2))
  }
  else{ 
    return(sqrt(a^2 + b^2))
  }
}

#' @title area of a circle
#' @description gives the area of a circle
#' @param radius radius of the circle. The radial value may only be positive (numeric)
#' @return An area of the circle. Calculated as A = pi*r^2(numeric)

circle_area <- function(radius = FALSE){
  if(radius == FALSE){
    return(pi)
  }
  if(radius == 0){
    stop("The radius must be non-zero")
  }
  if(radius < 0){
    stop("The radius must be non-negative")
  }
  else{
    return(pi*(radius)^2)
  }
}

#' @title Area of a cylinder
#' @description This function is used to calculate 
#' the area of a cylinder. It uses a function call to the
#' function circle_area
#' @param radius the radial component of the cylinder (numeric)
#' @param height the height of the cylinder. Must be a positive value (numeric)
#' @return the area of a cylinder (numeric)
cylinder_area <- function(radius = FALSE, height = FALSE){
  if(height < 0){
    stop("Please enter a valid height")
  }
  if(radius == FALSE & height == FALSE){
    return(2 * pi  + 2 * circle_area())
  }  
  if(height == FALSE){
    return(2 * pi *radius + 2 * circle_area(radius))
  }
  if(radius == FALSE){
    return(2 * pi *height + 2 * circle_area())
  }
  else{
    return(2 * pi *radius * height + 2 * circle_area(radius))
  }
}


#' @title Volume of a cylinder
#' @description This function is used to calculate 
#' the volume of a cylinder. It uses a function call to the
#' function circle_area
#' @param radius the radial component of the cylinder (numeric)
#' @param height the height of the cylinder. Must be a positive value (numeric)
#' @return the volume of a cylinder (numeric)
cylinder_volume <- function(radius = FALSE, height = FALSE){
  if(height < 0){
    stop("Please enter a valid height")
  }
  if(radius == FALSE & height == FALSE){
    return(circle_area())
  }  
  if(height == FALSE){
    return(circle_area(radius))
  }
  if(radius == FALSE){
    return(height* circle_area())
  }
  else{
    return(height * circle_area(radius))
  }
}

#' @title Miles to Kilometers convertor
#' @description Function to convert a given number
#' of miles to its corresponding number of kilometers
#' @param miles. Number of miles to be converted (numeric)
#' @return kilometers. Converted number of miles (numeric)
miles2kms <- function(miles = FALSE){
  if(miles == FALSE){
    return(1.6)
  }
  else{
  return(miles * 1.6)
  }
}

# to create the data table
conversiontablemiles <- data.frame("Miles" = c(1:100), Kilometers = miles2kms(c(1:100)))

#' @title Gallons to Liters convertor
#' @description Function to convert a given number
#' of gallons to its corresponding number of liters
#' @param gallons. Number of gallons to be converted (numeric)
#' @return liters. Converted number of liters (numeric)
gallons2liters <- function(gallons = FALSE){
  if(gallons == FALSE){
    return(3.78541)
  }
  else{
    return(gallons * 3.78541)
  }
}

#' @title Liter to Gallon convertor
#' @description Function to convert a given number
#' of liters to its corresponding number of gallons
#' @param liters. Number of liters to be converted (numeric)
#' @return gallons. Converted number of gallons (numeric)
liters2gallons <- function(liters = FALSE){
  if(liters == FALSE){
    return(1/gallons2liters())
  }
  else{
    return(liters/gallons2liters())
  }
}

# to create the data table
conversiontableliters <- data.frame("liters" = c(c(1:9),seq(10,100,10)), 'gallons' = liters2gallons(c(c(1:9),seq(10,100,10))))


#' @title Seconds to years
#' @description Converts a given number of seconds to 
#' its corresponding number of years
#' @param seconds. Given number of seconds (numeric)
#' @return years. Corresponding number of years (numeric)
seconds2years <- function(seconds = FALSE){
  if(seconds == FALSE){
    return(1/(365*24*3600))
  }
  else{
    return(seconds/(365*24*3600))
  }
}
# Yes, a newborn can expect to live for 1 billion seconds

#' @title Searching for the zeros of polynomials
#' @description We will be looking at three different
#' polynomials and seeing if the graph of these polynomails
#' cross the x axis (if there is a value where the function is zero)
#' @param x. independent variable (numeric)
#' @return f. dependent variable (numeric)
poly1 <- function(x){
  return(x ^ 3)
}
poly2 <- function(x){
  return((x^2 - 1)*(x+3)^3)
}
poly3 <- function(x){
  return((x^2 - 1)*(x^2 - 9))
}
# plots for the 3 polynomials
x <- seq(-4, 4, length.out = 20)
plot(x, poly1(x), type = 'l', lwd = 3, col = "#FB7215", las = 1)
abline(h = 0, v = 0, col = '#888888aa', lwd = 1.5)
title(main = expression(paste(f(x), ' = ', x^3)))

plot(x, poly2(x), type = 'l', lwd = 3, col = "#FB7215", las = 1)
abline(h = 0, v = 0, col = '#888888aa', lwd = 1.5)
title(main = expression(paste(f(x), ' = ', (x^2-1)(x+3)^3)))

plot(x, poly3(x), type = 'l', lwd = 3, col = "#FB7215", las = 1)
abline(h = 0, v = 0, col = '#888888aa', lwd = 1.5)
title(main = expression(paste(f(x), ' = ', (x^2 - 1)(x^2 - 9))))

#' @title interval guardian
#' @description Checks if a given value falls within
#' the interval[0,100]. If it doesn't, then it is shrunk
#' to fit within the interval
#' @param x. Real valued variable (numeric)
#' @return y. Value in interval [0,100] (numeric)

interval_guardian <- function(x){
  if(x > 100){
    return(100)
  }
  else if(x < 0){
    return(0)
  }
  else{
    return(x)
  }
}
      
#'@title Even number checker
#'@description Checks whether a given numerical value
#'is even or odd. Even values will return TRUE, while
#'odd values will return FALSE. Character values will 
#'return 'NA'
#'@param x. Integer value (numeric)
#'@return y. Boolean value (BOOL)
is_even <- function(x){
  if(is.character(x) == TRUE){
    return(NA)
  }
  
  else if( x%%1 != 0){
    return("Please enter only integer values")
  }
  else{
    if(x %% 2 == 0){
      return(TRUE)
    }
    else{
      return(FALSE)
      }
  }
}      
      
#'@title Odd number checker
#'@description Checks whether a given numerical value
#'is even or odd. Odd values will return TRUE, while
#'even values will return FALSE. Character values will 
#'return 'NA'
#'@param x. Integer value (numeric)
#'@return y. Boolean value (BOOL)
is_odd <- function(x){
  if(is.character(x) == TRUE){
    return(NA)
  }
  
  else if( x%%1 != 0){
    return("Please enter only integer values")
  }
  else{
    if(x %% 2 != 0){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
} 

#'@title Grade convertor
#'@description Takes a numerical value and returns the 
#'letter grade the corresponds to that given numerical
#'value
#'@param score. Integer value (numeric)
#'@return y. Letter grade (character)
grade <- function(score){
  if(score > 100 | score < 0){
    stop("Please enter a valid score. The score must be a number between 0 and 100.")
  }
  else{
  if(90 <= score  & score <= 100){
    return("A")
} else if(80 <= score & score < 90){
    return("B")
} else if (70 <= score & score < 80){
    return("C")
} else if(60 <= score & score < 70){
    return("D")
} else if(score < 60){
    return("F")
}
  }
}

#' @title Miles to inches convertor
#' @description Function to convert a given number
#' of miles to its corresponding number of inches
#' @param miles. Number of miles to be converted (numeric)
#' @return inches. Converted number of miles (numeric)
miles2inches <- function(miles = FALSE){
  if(miles == FALSE){
    return(63360)
  }
  else{
    return(miles * 63360)
  }
}

#' @title Miles to feet convertor
#' @description Function to convert a given number
#' of miles to its corresponding number of feet
#' @param miles. Number of miles to be converted (numeric)
#' @return feet. Converted number of miles (numeric)
miles2feet <- function(miles = FALSE){
  if(miles == FALSE){
    return(5280)
  }
  else{
    return(miles * 5280)
  }
}

#' @title Miles to yards convertor
#' @description Function to convert a given number
#' of miles to its corresponding number of yards
#' @param miles. Number of miles to be converted (numeric)
#' @return yards. Converted number of miles (numeric)
miles2yards <- function(miles = FALSE){
  if(miles == FALSE){
    return(1760)
  }
  else{
    return(miles * 1760)
  }
}

#' @title Miles to meters convertor
#' @description Function to convert a given number
#' of miles to its corresponding number of meters
#' @param miles. Number of miles to be converted (numeric)
#' @return meters. Converted number of miles (numeric)
miles2meters <- function(miles = FALSE){
  if(miles == FALSE){
    return(1/0.00062137)
  }
  else{
    return(miles /0.00062137)
  }
}


#' @title Miles to kilometers convertor
#' @description Function to convert a given number
#' of miles to its corresponding number of kilometers
#' @param miles. Number of miles to be converted (numeric)
#' @return kilometers. Converted number of miles (numeric)
miles2kms <- function(miles = FALSE){
  if(miles == FALSE){
    return(1/0.62137)
  }
  else{
    return(miles /0.62137)
  }
}

convert <- function(x,to){
  switch(to, # The expression to be evaluated.
         "in" = return(miles2inches(x)),
         "ft" = return(miles2feet(x)),
         "yd" = return(miles2yards(x)),
         "m" = return(miles2meters(x)),
         "km" = return(miles2kms(x)),
         return(miles2kms(x)))
}