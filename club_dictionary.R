#club_name_pl - team names from https://www.premierleague.com/clubs
club_name_pl <- c("Arsenal", "AFC Bournemouth", "Brighton and Hove Albion", 
                "Burnley", "Cardiff City", "Chelsea", "Crystal Palace", 
                "Everton", "Fulham", "Huddersfield Town", "Leicester City", 
                "Liverpool", "Manchester City", "Manchester United", 
                "Newcastle United", "Southampton", "Tottenham Hotspur",
                "Watford", "West Ham United", "Wolverhampton Wanderers")

#club_name_fpl - team names in FPL
club_name_fpl <- c("Arsenal", "Bournemouth", "Brighton", "Burnley", "Cardiff", 
                   "Chelsea", "Crystal Palace", "Everton", "Fulham", "Huddersfield", 
                   "Leicester", "Liverpool", "Man City", "Man Utd", "Newcastle", 
                   "Southampton", "Spurs", "Watford", "West Ham", "Wolves")

#club_name_wfn - team names from http://www.worldfootball.net/all_matches/eng-premier-league-2018-2019/
club_name_wfn <- c("Arsenal FC", "AFC Bournemouth", "Brighton & Hove Albion", 
                   "Burnley FC", "Cardiff City", "Chelsea FC", "Crystal Palace", 
                   "Everton FC", "Fulham FC", "Huddersfield Town", "Leicester City", 
                   "Liverpool FC", "Manchester City", "Manchester United", 
                   "Newcastle United", "Southampton FC", "Tottenham Hotspur",
                   "Watford FC", "West Ham United", "Wolverhampton Wanderers")

#club_id_pl - id from https://www.premierleague.com/clubs
club_id_pl <- c(1, 127, 131, 43, 46, 4, 6, 7, 34, 159, 26, 10, 11, 
               12, 23, 20, 21, 33, 25, 38)

#club_id_fpl - club id in FPL
club_id_fpl <- c(3, 91, 36, 90, 97, 8, 31, 11, 54, 38, 13, 14, 43, 1, 4, 20, 6,
                 57, 21, 39)

club_dict <- data.frame(club_name_pl, club_name_fpl, club_id_pl, club_id_fpl)

saveRDS(club_dict, "club_dict.rds")

