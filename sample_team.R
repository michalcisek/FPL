sample_team <- function(){
  team <- character(15)
  for (i in 1:15) {
    #sample position of player that will be sampled...
    position_limit <- FALSE
    while (position_limit == FALSE){
      position <- sample_position()
      if(check_position_limit() != 'exceeded') position_limit <- TRUE
    }
    
    #sample player
    team_value <- FALSE
    team_limit <- FALSE
    while(team_value == FALSE & team_limit == FALSE){
      player <- sample_player()
      tv <- check_team_value()
      tl <- check_team_limit()
      if(tv != "exceeded" & tl != "exceeded"){
        team_value <- TRUE
        team_limit <- TRUE
      } 
    }
    
    #add sampled player to the team
    team <- c(team, player)
  }
  return(team)
}