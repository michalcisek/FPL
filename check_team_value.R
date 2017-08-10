check_team_value <- function(player, team, data, approach = "balanced", tolerance = 1){
  
  if(approach == "balanced") limit <- c(9, 25.5, 37, 28.5)
  else if(approach == "midfield") limit <- c(8.5, 23.5, 43.5, 24.5)
  else if(approach == "forward") limit <- c(8.5, 24, 33, 34.5)
  
  
  new_team <- c(team, player)
  sum_value <- data[data$code %in% new_team, "cost"] %>% sum 
  
  pos_df <- data.frame(pos = data[data$code %in% new_team, "pos"], cost = data[data$code %in% new_team, "cost"])
  by(pos_df$cost, pos_df$pos, sum) %>% 
    c %>% 
    data.frame(pos = names(.), value = .) -> pos_exp 
    
  limits <- data.frame(pos = c("GKP", "DEF", "MID", "FWD"), limit = limit,
                       avg_limit = limit/c(2, 5, 5, 3)*tolerance)
  limits <- merge(pos_exp, limits, by = "pos") 
  limits <- limits[complete.cases(limits), ]
  
  data[data$code %in% new_team, "pos"] %>% 
    table %>% 
    c %>% 
    data.frame(pos = names(.), number = .) -> pos_number
  
  limits <- merge(limits, pos_number, by = "pos")
  limits <- limits %>% mutate(avg_value = value/number)
  
  if(sum_value >= 100) return("exceeded")
  else if(any(limits$value > limits$limit) | any(limits$avg_value > limits$avg_limit)) return("exceeded")
  else return("not exceeded")
}