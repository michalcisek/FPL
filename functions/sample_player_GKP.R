sample_player_GKP <- function(data, type = "only_played"){
  if(type == "only_played") code <- sample(data$code, 1, prob = data$probs)  
  else if(type == "include_fresh"){
    dec <- sample(c("played", "fresh"), 1, prob = c(.9, .1))
    
    code <- sample(data[[dec]]$code, 1, prob = data[[dec]]$probs)
  }
  
  return(code)
}