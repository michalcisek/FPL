check_team_limit <- function(player, team, data){
  used <- data[data$code %in% team, "team"]
  team_sum <- sum(used == data[data$code == player, "team"])
  
  if(team_sum >= 3) return("exceeded")
  else return("not exceeded")
}