gaussian <- function(s,m,x){
  if(s <= 0){
    stop("Error. Please enter valid parameter values")

  }
  else{
  return((1/(s*sqrt(2*pi)))*exp(-.5*((x-m)/s)^2))
  }
}

#gaussian curve
x_values <- seq(-4.5,4.5,0.1)
y_values <- gaussian(2,0,x_values)
plot(x_values, y_values, las = 1, type = "l", lwd = 2)


##### Descriptive statistics #####
descriptive <- function(input,na.rm = FALSE){
  info = list("min" = min(input),
              "first quartile" =  quantile(input, 0.25),
              "median" = median(input),
              "mean" = mean(input),
              "thrid quartile" = quantile(input, 0.75),
              "max" = max(input),
              "range" = max(input) - min(input),
              "interquartile range" = IQR(input),
              "standard deviation" = sd(input))
  return(info)
}

##### Two Given Points #####
find_distance <- function(p1,p2){
  return(sqrt((p2[2] - p1[2])^2 + (p2[1] - p1[1])^2))
}

find_midpoint <- function(p1,p2){
  return(c((p1[1] + p2[1])/2, (p2[1] + p2[2])/2))
}

find_slope <- function(p1,p2){
  return((p2[2] - p1[2])/(p2[1] - p1[1]))
}

find_intercept <- function(p1,p2){
  return(p1[2] - find_slope(p1,p2)*p1[1])
}

find_line <- function(p1,p2){
  return(list("intercept" = find_intercept(p1,p2),
              "slope" = find_slope(p1,p2)))
}

info_points <- function(p1,p2){
  return(list("distance" = find_distance(p1,p2),
              "midpoint" = find_midpoint(p1,p2),
              "intercept" = find_intercept(p1,p2),
              "results" = find_slope(p1,p2)))
  
}

p1 <- c(-2, 4)
p2 <- c(1, 2)

plot.new()
# depending on your chosen points you may have to set different limits
plot.window(xlim = c(-3, 3), ylim = c(0, 5))
axis(side = 1)
axis(side = 2, las = 1)
points(p1[1], p1[2], cex = 1.5, col = "#FF8834", pch = 19)
points(p2[1], p2[2], cex = 1.5, col = "#FF8834", pch = 19)
# midpoint (here you should use the midpoint outputs of your function)
points(-1/2, 3, cex = 1.5, pch = "x", col = "#E16868")
# slope and intercept (here you should use the outputs of your function)
abline(a = 8/3, b = -2/3, col = "#FF883477", lwd = 3)
title(main = expression(paste(y, ' = ', (-2/3) * x, ' + ', (8/3))))