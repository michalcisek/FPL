#club_name_pl - team names from https://www.premierleague.com/clubs
club_name_pl <- c("Arsenal", "AFC Bournemouth", "Brighton and Hove Albion", 
                "Burnley", "Chelsea", "Crystal Palace", 
                "Everton", "Leicester City", 
                "Liverpool", "Manchester City", "Manchester United", 
                "Newcastle United", "Southampton", "Tottenham Hotspur",
                "Watford", "West Ham United", "Wolverhampton Wanderers",
                "Aston Villa", "Sheffield United", "Norwich City")

#club_name_fpl - team names in FPL
club_name_fpl <- c("Arsenal", "Bournemouth", "Brighton", "Burnley",  
                   "Chelsea", "Crystal Palace", "Everton",  
                   "Leicester", "Liverpool", "Man City", "Man Utd", "Newcastle", 
                   "Southampton", "Spurs", "Watford", "West Ham", "Wolves",
                   "Aston Villa", "Sheffield Utd", "Norwich")

#club_name_wfn - team names from http://www.worldfootball.net/all_matches/eng-premier-league-2019-2020/
club_name_wfn <- c("Arsenal FC", "AFC Bournemouth", "Brighton & Hove Albion", 
                   "Burnley FC", "Chelsea FC", "Crystal Palace", 
                   "Everton FC", "Leicester City", 
                   "Liverpool FC", "Manchester City", "Manchester United", 
                   "Newcastle United", "Southampton FC", "Tottenham Hotspur",
                   "Watford FC", "West Ham United", "Wolverhampton Wanderers",
                   "Aston Villa", "Sheffield United", "Norwich City")

#club_id_pl - id from https://www.premierleague.com/clubs
club_id_pl <- c(1, 127, 131, 43, 4, 6, 7, 26, 10, 11, 
               12, 23, 20, 21, 33, 25, 38,
               2, 18, 14)

#club_id_fpl - club id in FPL - check in fplr package
club_id_fpl <- c(3, 91, 36, 90, 8, 31, 11, 13, 14, 43, 1, 4, 20, 6,
                 57, 21, 39,
                 7, 49, 45)

club_dict <- data.frame(club_name_pl, club_name_fpl, club_id_pl, club_id_fpl)

saveRDS(club_dict, "club_dict.rds")

