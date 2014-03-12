fib.calc <- compiler::cmpfun( function(n) {
  fib.cache <- new.env(hash=T, parent=emptyenv()) 
  f <- function(n) {
    if (n > 1) {
      if (is.null(fib.cache[[toString(n)]])) {
        fib.cache[[toString(n)]] <- Recall(n-1) +
          Recall(n-2)
      }
      fib.cache[[toString(n)]]
    } else n  
  }
  f(n)
})
