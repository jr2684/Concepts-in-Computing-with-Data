device <- function(sides = c("1","2"), prob = c(0.5,0.5)){
  object <- list(sides = sides,
                 prob = prob)
  attr(object,"prob") <- prob
  class(object) <- "device"
  object
}

print.device <- function(x) {
  cat('object "device"\n\n')
  cd <- data.frame(
    side = x$sides, prob = x$prob
  )
  print(cd)
  invisible(x)
}

fair_coin <- device()
#die <- device(sides <- c('1', '2', '3', '4'), prob <- c(0.25, 0.25, 0.25, 0.25))
