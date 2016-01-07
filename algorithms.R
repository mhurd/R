fermatPrime <- function(p, max_tests) {
  if (p ==1) {
    return(TRUE)
  }
  i <- 0
  repeat {
    n =  round(runif(1, 1, p-1))
    print(paste("Using ", n, " as the random seed"))
    p1 <- n^(p-1)
    print(paste("(n^(p-1) = ", p1))
    p2 <- p1 %% p
    print(paste("(n^(p-1) %% p = ", p2))
    if (p2 != 1) {
      print(paste(p, " is not prime!"))
      return(FALSE)  
    } 
    i <- i + 1
    if (i > max_tests) break()
  }
  print(paste("There is a ", 1/(2^max_tests), " chance that ", p, " is not prime."))
  return(TRUE)
}