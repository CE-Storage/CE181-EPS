calculate <- function() {
  data = read.csv('data.csv')
  x <- data$x
  y <- data$y
  n <- length(x)
  a <- sum(x*y) / (1 + sum(x^2))
  b <- (sum(x)*sum(y)) / ((n+1)*(1 + sum(x^2)))
  c <- (sum(x)^2) / ((n + 1) * (1 + sum(x^2)))
  alpha <- (a - b) / (1 - c)
  betha <- (sum(y) - alpha*sum(x)) / (n + 1)
  y.hat <- alpha*x + betha
  plot(x, y)
  lines(x, y.hat, col = 'blue')
  return(c(alpha, betha))
}
