

library(data.table)


shuffle <- function(x, y){
  len_x <- length(x)
  len_y <- length(y)
  conc <- c(x, y)
  
  perm <- sample(conc, size = length(conc), replace = FALSE)
  
  x <- perm[1:len_x]
  y <- perm[len_x+1:len_y]
  
  list(x=x, y=y)
}

draw_bs <- function(x, y, FUN = mean, size=100, ...){
  out <- vector('double', size)
  shuffled <- shuffle(x, y)
  x <- shuffled[['x']]
  y <- shuffled[['y']]
  for (i in 1:size){
    xbs <- sample(x, replace=TRUE)
    ybs <- sample(y, replace=TRUE)
    out[i] <- FUN(xbs, ...) - FUN(ybs, ...)
  }
  out
}

set.seed(2323)

x1 <- rnorm(1000, mean = 9.5, sd = 3)
x2 <- rnorm(1000, mean = 10, sd = 3)

t.test(x1, x2, alternative = "less")


diff <- mean(x1) - mean(x2)
mean(diff >= draw_bs(x1, x2, mean, 100000, na.rm=TRUE))
























