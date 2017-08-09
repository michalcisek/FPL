check_position_limit <- function(position, team, data){
  
  dict <- data.frame(positions = c("GKP", "DEF", "MID", "FWD"), limit = c(2, 5, 5, 3))
  
  #number of players from different positions already used in team
  used <- data[data$code %in% team, "pos"] 
  pos_sum <- sum(used == position)
    
  if(pos_sum >= dict[dict$positions == position, "limit"]) return("exceeded")
  else return("not exceeded")
}
