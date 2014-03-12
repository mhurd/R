longCallOptionValue <- function(strike, priceAtMaturity) {
  max((priceAtMaturity - strike), 0)
}

longPutOptionValue <- function(strike, priceAtMaturity) {
  max((strike - priceAtMaturity), 0)
}

shortCallOptionValue <- function(strike, priceAtMaturity) {
  # Remember this option is sold so the buyer will only exercise it
  # if the price has risen - cost to us is at best 0
  min(strike - priceAtMaturity, 0)
}

shortPutOptionValue <- function(strike, priceAtMaturity) {
  # Remember this option is sold so the buyer will only exercise it
  # if the price has risen - cost to us is at best 0
  min(priceAtMaturity - strike, 0)
}

longCallOptionProfits <- function(strike, maturityPrices, premium) {
  profits <- c(length=length(maturityPrices))
  for (i in 1:length(maturityPrices)) {
    profits[i] <- longCallOptionValue(strike, maturityPrices[i])-premium
  }
  return(profits)  
}

shortCallOptionProfits <- function(strike, maturityPrices, premium) {
  profits <- c(length=length(maturityPrices))
  for (i in 1:length(maturityPrices)) {
    profits[i] <- shortCallOptionValue(strike, maturityPrices[i])-premium
  }
  return(profits)  
}

optionGraph <- function(maturityPrices) {
  scale<-range(maturityPrices)
  plot(scale, c(-max(scale), max(scale)), type="n", xlab="Price at Maturity", ylab="Profit $") 
}

plotLongCallOption <- function(strike, maturityPrices, premium, colour) {
  data <- longCallOptionProfits(strike, maturityPrices, premium)
  lines(maturityPrices, data, type="l", col=colour)
}

plotShortCallOption <- function(strike, maturityPrices, premium, colour) {
  data <- shortCallOptionProfits(strike, maturityPrices, premium)
  lines(maturityPrices, data, type="l", col=colour)
}

plotLongCallButterfly <- function(strike, offset, maturityPrices, premium) {
  optionGraph(maturityPrices)
  points(x=strike, y=0, type="o", pch = 21, bg = "orange")
  points(x=strike-offset, y=0, type="o", pch = 21, bg = "red")
  points(x=strike+offset, y=0, type="o", pch = 21, bg = "green")
  plotLongCallOption(strike-offset, maturityPrices, premium, "blue")
  plotShortCallOption(strike, maturityPrices, premium, "red")
  plotShortCallOption(strike, maturityPrices, premium, "green")
  plotLongCallOption(strike+offset, maturityPrices, premium, "purple")
}