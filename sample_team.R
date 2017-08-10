sample_team <- function(data, GKP, DEF, MID, FWD, type, approach, tolerance){

  team <- character(15)
    
  for (i in 1:15){
    #sample position of player that will be sampled...
    position_limit <- FALSE
    while (position_limit == FALSE){
      position <- sample_position()
      if(check_position_limit(position, team, data) != 'exceeded') position_limit <- TRUE
    }
    
    #sample player
    team_value <- FALSE
    team_limit <- FALSE
    niter <- 0
    while((team_value != TRUE & team_limit != TRUE) & niter < 100){
      player <- eval(parse(text = paste0("sample_player_", position, "(data = ", 
                                         tolower(position), ", type = '", type, "')")))

      tv <- check_team_value(player, team, data, approach, tolerance)
      tl <- check_team_limit(player, team, data)
      if(tv == "not exceeded" & tl == "not exceeded"){
        team_value <- TRUE
        team_limit <- TRUE
      } 
      niter <- niter + 1
    }
    
    if(niter == 100){
      team[i] <- NA
    } else{
      #remove selected player from dataset
      if(type == "only_played"){
        eval(parse(text = paste0(tolower(position), " <- " , 
                               tolower(position),"[-which(", tolower(position), "$code == ", player, "), ]")))
      } else if(type == "include_fresh"){
        if(player %in% eval(parse(text = paste0(tolower(position), "[[1]]$code")))){
          eval(parse(text = paste0(tolower(position), "[[1]] <- " , 
                                   tolower(position),"[[1]][-which(", tolower(position), "[[1]]$code == ", player, "), ]")))
        } else{
          eval(parse(text = paste0(tolower(position), "[[2]] <- " , 
                                   tolower(position),"[[2]][-which(", tolower(position), "[[2]]$code == ", player, "), ]")))
        }        
      }
     
      #add sampled player to the team
      team[i] <- player
    }  
  }
  
  if(any(is.na(team))){
    warning("Couldn't sample whole team!")
    return(NULL)
  } else{
    return(team)
  }  
}