diceThrowsHist <- function(n) {
  hist(breaks=seq(0,6,1),sample(1:6,n,replace=T), col=colors, main=paste(n," Dice Throws"), xlab="Dice Throw")
}