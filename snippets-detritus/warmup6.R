#' @title Integer test function
#' @description This function tests whether a 
#' given number is an integer value. If the value in
#' question is an integer, then the function will
#' return TRUE. If it is not, FALSE will be returned
#' @param x. numerical value (numeric)
#' @return y. Truth value about x (BOOL)

is_integer <- function(x){
  if(x %% 1 == 0){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#' @title Positivity checker
#' @description Checks whether a numerical value is 
#' greater than zero or less than zero.
#' @param x. numerical value in question (numeric)
#' @param y. Truth value about x (BOOL)

is_positive <- function(x){
  if(x > 0){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#' @title Inclusive positivity checker
#' @description This function checks whether a 
#' numerical value is non-negative. This differs
#' from the function is_positive in that the value 
#' 0 evaluates to TRUE instead of FALSE
#' @param x. Numerical value in questions (numeric)
#' @param y. Truth value about x (BOOL)

is_nonnegative <- function(x){
  if(x >= 0){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#' @title Positive integer checker
#' @description Checks whether a given numerical value
#' is both an integer and positive. The function
#' uses the functions is_positve and is_integer in 
#' establishing the truth value of the numerical
#' value in questions
#' @param x. The numerical value in question (numeric)
#' @param y. A truth value of whether x is both an 
#' integer and positive (BOOL)

is_positive_integer <- function(x){
  if(is_integer(x) == TRUE & is_positive(x) == TRUE){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#'@title Nonnegative integer checker
#'@description Checks whether a numerical value
#'is both nonnegative and an integer. Utilizes the
#'functions is_nonnegative and is_integer
#'@param x. Numerical value in question (numeric)
#'@return y. Truth value of x (BOOL)

is_nonneg_integer <- function(x){
  if(is_nonnegative(x) == TRUE & is_integer(x) == TRUE){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

#' @title Valid probability value checker
#' @description This subroutine checks whether a 
#' given value is a valid probability value. In 
#' particular, it checks whether a value is inside of
#' the interval [0,1]
#' @param x. Numerical value in question (numeric)
#' @return y. Truth value of x (BOOL)

is_probability <- function(x){
  if(x >= 0 & x <= 1){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#' @title Factorial function
#' @description Calculates the factorial of a given integer
#' The factorial of a function is given as n! = n(n-1)(n-2)...1
#' @param x. An integer value (numeric)
#' @return y. The factorial of x (numeric)

bin_factorial <- function(x){
  if(x == 0){
    return(1)
  } else{
  total = 1
  for(i in 1:x){
    total <- total * i
    }
  return(total)
  }
}

#' @title n choose k function
#' @description Calculates the values of k 
#' successes in n trials. This function calls upon
#' the function bin_factorial()
#' @param k. number of successes (numeric)
#' @param n. number of possible trials (numeric)
#' @return y. number of k successes that can occure
#' in n possible trials (numeric)

bin_combinations <- function(n,k){
  return(bin_factorial(n)/(bin_factorial(k) * bin_factorial(n-k)))
}

#' @title Binomial Probability
#' @description Calculates the probability of
#' getting k successes in n trials
#' @param n. Number of trials (numeric)
#' @param k. Number of successes (numeric)
#' @param prob. Probability of success (numeric)
#' @return y. Probability of getting the k successes (numeric)

bin_probability <- function(trials, success, prob){
  if(success > trials){
    stop("Invalid number of trials")
  }
  else if(is_nonneg_integer(trials) == FALSE | is_nonneg_integer(success) == FALSE){
    if(is_nonneg_integer(trials) == FALSE){
      stop("Invalid trials value")
    }else{
      stop("Invalid success value")
    }
  }
  else if(is_probability(prob) == FALSE){
        stop("Invalid probability value")
      }
  else{
    return(bin_combinations(trials, success) * (prob^success) * ((1-prob)^(trials-success)))
  }
}


#'@title Binomial distribution
#'@description creates a distribution, given
#'a number of trials and a probability value,
#'of different success rates
#'@param trials. number of trials (numeric)
#'@param prob. Probability of success for a trial (numeric)
#'@return dataframe of distributions (list)

bin_distribution <- function(trials, prob){
  probability <- rep(0,trials+1)
  success <- seq(0,trials,1)
  for( i in 0:(length(success)-1)){
    print(i)
    print(trials)
    probability[i+1] <- bin_probability(trials, i, prob)
    print(probability[i+1])
  }
  return(data.frame("success" = success, "probability" = probability))
}