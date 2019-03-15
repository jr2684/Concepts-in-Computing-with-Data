library(ggplot2)

bag1 <- c('white', 'white', 'red')
bag2 <- c(rep('white',3), 'red')
bags <- c('bag1', 'bag2')
repetitions <- 1000
drawn_balls <- character(repetitions)

set.seed(345)
for(i in 1:repetitions){
  #select 1 bag
  chosen_bag <- sample(bags,1)
  
  #draw a ball from chosen bag
  if(chosen_bag == 'bag1'){
    drawn_balls[i] <- sample(bag1, 1)
  } else{
    drawn_balls[i] <- sample(bag2, 1)
  }
}
##### less basic problem #####
set.seed(2345)
n <- 1000
drawn_balls <- matrix(0,nrow = n,ncol = 4)
box1 <- c(rep('blue',2), 'red')
box2 <- c(rep('blue',2), rep('red', 3), 'white')
for(i in 1:n){
  if(runif(1) > 0.5){
    drawn_balls[i,] <- sample(box1, size = 4, replace = TRUE)
  } else{
    drawn_balls[i,] <- sample(box2, size = 4, replace = TRUE)
  }
}
occurences <- rep(0,n)
for(i in 1:n){
occurences[i] <- sum((drawn_balls[i,]=="blue")==TRUE)
}
#occurences <- sum((drawn_balls=="blue")==TRUE)
freq_0 <- cumsum(occurences == 0) / 1:n
freq_1 <- cumsum(occurences == 1) / 1:n
freq_2 <- cumsum(occurences == 2) / 1:n
freq_3 <- cumsum(occurences == 3) / 1:n
freq_4 <- cumsum(occurences == 4) / 1:n
n_series = seq(1:n)
frequencies <- data.frame(n_series,"0" = freq_0,"1" = freq_1, "2" = freq_2,"3" = freq_3,"4" = freq_4)
#ggplot2() +qplot(seq_along(frequencies$freq_0), frequencies$freq_0)

ggplot(frequencies, aes(n_series, y = value, color = number)) +
  geom_line(aes(y = freq_0, col = "0")) +
  geom_line(aes(y = freq_1, col = "1")) +
  geom_line(aes(y = freq_2, col = "2")) +
  geom_line(aes(y = freq_3, col = "3")) +
  geom_line(aes(y = freq_4, col = "4")) +
  ggtitle("Relative frequencies of the number of blue balls") +
  xlab("reps") + ylab("freq.") 