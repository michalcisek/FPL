sample_position <- function(){
  positions <- c("GKP", "DEF", "MID", "FWD")
  #probabilities based on the number of available players selected in FPL
  probs <- c(2/15, 1/3, 1/3, 1/5)
  
  position <- sample(positions, 1, prob = probs)  
  
  return(position)
}