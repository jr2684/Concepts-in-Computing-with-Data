### For Loops ###
vec <- c(3,1,4)
for(j in 1:length(vec)){
  vec[j] <- vec[j]*3
}

vec2 <- rep(0,length(vec))
for(i in 1:length(vec)){
  vec2[j] <- vec[j]*3
}

### Summation Series ###
sum = 0
n = 1000
for(i in 1:n){
  sum = sum + (1/(2^(i-1)))
}
print(sum)

sum = 0
n = 60
for(i in 1:n){
  sum = sum + (1/(9^(i-1)))
}
print(sum)

### Arithmetic Series ###
total = 0
n = 100
a1 = 3
d = 3
series = rep(0,n)
for(i in 1:n){
  total = a1 + (i-1)*d
  series[i] = total
}
print(series)
# does not converge

#####geometric sequence #####
n = 20
geom = rep(0,n)
a1 = 3
r = 2
for( i in 1:n){
  geom[i] <- a1*r^(i-1)
}
sum(geom)

##### sine approximation ####
point = 10
total = 0
n = 100
for( i in 1:n){
  total = total + (point^((2*i)-1)/factorial((2*i)-1))*((-1)^(i-1))
  }
print(total)

##### for loop with a matrix #####
X <- matrix(rnorm(12), nrow = 4, ncol = 3)
print(X)
for(i in 1:nrow(X)){
  for(j in 1:ncol(X)){
    if(X[i,j] >= 0){
      X[i,j] <-  sqrt(X[i,j])
    }
    else{
      X[i,j]  <-  (X[i,j])^2
    }
  }
}
print(X)

##### Dividing a number by 2 multiple times #####

#' @title Reduction function
#' @description The scalar value will be continuously
#' reduced until the value becomes odd. This odd value 
#' will then be returned
#' @param x. Even numerical value (numeric)
#' @return y. Odd numerical value (numeric)

reduce <- function(x){
  while(x%%2 == 0){
    x <-  x/2
  }
  return(x)
}

##### average #####
vector = seq(1,100,1)
mean_for = 0
mean_while = 0
mean_repeat = 0
for(i in 1:length(vector)){
  mean_for = mean_for + vector[i]
}
for_mean = mean_for/length(vector)
cat("for loop mean: ", for_mean)
temp = 0
for(i in 1:length(vector)){
  temp = temp + (vector[i] - for_mean)^2
}
cat("for loop SD ", sqrt((1/(length(vector)-1))*temp))

i = 1
while(i <= length(vector)){
  mean_while <- mean_while + vector[i]
  i <- i + 1
}
while_mean = mean_while/length(vector)
cat("while loop: ", while_mean)

temp = 0
i = 1
while(i <= length(vector)){
  temp <- temp + (vector[i] - while_mean)^2
  i <- i + 1
  }
cat("while loop SD ", sqrt((1/(length(vector)-1))*temp))



j = 1
repeat{
  if(j < 101){
    mean_repeat <- mean_repeat + vector[j]
    j <- j + 1
  }
  else{
    break
  }
}
repeat_mean <- mean_repeat/length(vector)
cat("repeat loop mean: ", repeat_mean)

j = 1
temp = 0
repeat{
  if(j < 101){
    temp <- temp + (vector[j] - repeat_mean)^2
    j <- j + 1
  }
  else{
    break
  }
}
cat("repeat loop SD: ",sqrt((1/(length(vector)-1))*temp))

##### Geometric Mean #####
for_geo_mean = 1
vector = seq(1,50,1)
for(i in 1:length(vector)){
  for_geo_mean <-  for_geo_mean*vector[i]
}
for_geo_mean <- for_geo_mean^(1/length(vector))

while_geo_mean <-  1
i = 1
while(i <= length(vector)){
  while_geo_mean <- while_geo_mean * vector[i]
  i <- i + 1
}
while_geo_mean <- while_geo_mean^(1/length(vector))

repeat_geo_mean <- 1
j <- 1
repeat{if(j <= length(vector)){
  repeat_geo_mean <- repeat_geo_mean*vector[j]
  j <- j + 1
}
  else{
    break
  }
}
repeat_geo_mean <- (repeat_geo_mean)^(1/length(vector))

##### Distance Matrix Of Letters #####
# random distance matrix
num_letters <- length(LETTERS)
set.seed(123)
values <- sample.int(num_letters) 
distances <- values %*% t(values)
diag(distances) <- 0
dimnames(distances) <- list(LETTERS, LETTERS)

get_distance <- function(distance, letter_vec){
  for(i in 1:length(letter_vec)){
    if(letter_vec[i] %in% LETTERS == TRUE){
      
    }else{
      stop("Element is not a letter")
    }
  }
  sum = 0
  for(i in 1:(length(letter_vec)-1)){
    sum <- sum + distances[letter_vec[i],letter_vec[i+1]]
  }
  return(sum) 
}
cal <- c('C', 'A', 'L')
stats <- c('S', 'T', 'A', 'T', 'S')
oski <- c('O', 'S', 'K', 'I')
zzz <- rep('Z',3)
lets <- LETTERS
first <- c('J', 'O', 'S', 'H')
last <- c('R', 'E', 'I', 'N')
strings <- list(
  cal = cal,
  stats = stats,
  oski = oski,
  zzz = zzz,
  lets = lets, 
  first = first,
  last = last
)
strings_dist <- rep(0,length(strings))
counter = 1
for(i in strings){
  strings_dist[counter] <- get_distance(distances,i)
  counter <- counter + 1
  }
print(strings_dist)