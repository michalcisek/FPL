check_team_value <- function(player, team, data){
  team <- c(team, player)
  sum_value <- data[data$code %in% team, "cost"] %>% sum 
  
  if(team_value >= 100) return("exceeded")
  else return("not exceeded")
}