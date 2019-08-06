library(jsonlite)
library(fplr)


data <- fpl_get_bootstrap()$elements

data %>% 
  select(one_of('web_name', 'team_code', 'status', 'now_cost', 'selected_by_percent',
                'total_points', 'points_per_game', 'minutes', 'influence', 'creativity',
                'threat', 'element_type', 'code')) -> data

teams <- fpl_get_teams()

data <- sqldf::sqldf('select a.*, b.short_name, b.strength_overall_home, b.strength_overall_away,
             b.strength_attack_home, b.strength_attack_away, b.strength_defence_home,
             b.strength_defence_away from data a left join teams b on a.team_code=b.code')

positions <- fpl_get_bootstrap()$element_types


data <- data %>% left_join(positions, by = c("element_type" = "id"))

data %>%
  select(-one_of('team_code', 'element_type')) -> data

data %>% 
  select(one_of('web_name', 'short_name', 'singular_name_short')) -> desc

data %>% 
  select(-one_of('web_name', 'short_name', 'singular_name_short')) %>% 
  cbind(desc, .) -> data

data %>% 
  mutate(short_name = factor(short_name)) %>% 
  mutate(singular_name_short = factor(singular_name_short)) %>% 
  mutate(status = factor(status)) %>% 
  mutate(now_cost = now_cost/10) %>% 
  mutate(selected_by_percent = as.numeric(selected_by_percent)) %>% 
  mutate(points_per_game = as.numeric(points_per_game)) %>% 
  mutate(influence = as.numeric(influence)) %>% 
  mutate(creativity = as.numeric(creativity)) %>% 
  mutate(threat = as.numeric(threat)) -> data

colnames(data)[c(1:3, 5)] <- c("name", "team", "pos", "cost")

#save data of all players
saveRDS(data, "data.rds")
rm(desc, positions, teams)

data %>% 
  filter(total_points != 0, status != 'u') -> played

data %>% 
  filter(total_points == 0, status != 'u') -> fresh

# Data preperation according to position ----------------------------------

#goalkeepers
data %>% 
  filter(pos == "GKP", status != "u") %>% 
  select(-one_of('creativity', 'threat', 'status')) -> GKP

GKP %>% 
  select(selected_by_percent:strength_defence_away) %>% 
  select(-code) %>% 
  apply(., 2, function(x) x/sum(x)) %>% 
  cbind(cost = (1/GKP$cost)/sum(1/GKP$cost), .) %>% 
  apply(., 1, prod) -> probs

GKP %>% 
  select(name:cost, code) %>% 
  cbind(., probs) -> GKP

saveRDS(GKP, "GKP.rds")

data %>% 
  filter(pos == "GKP", status != "u", total_points == 0) %>% 
  select(-one_of('creativity', 'threat', 'status')) -> GKP

GKP %>% 
  select(selected_by_percent, strength_overall_home:strength_defence_away) %>% 
  apply(., 2, function(x) x/sum(x)) %>% 
  cbind(cost = (1/GKP$cost)/sum(1/GKP$cost), .) %>% 
  apply(., 1, prod) -> probs

GKP %>% 
  select(name:cost, code) %>% 
  cbind(., probs) -> GKP_fresh


data %>% 
  filter(pos == "GKP", status != "u", total_points != 0) %>% 
  select(-one_of('creativity', 'threat', 'status')) -> GKP

GKP %>% 
  select(selected_by_percent:strength_defence_away) %>% 
  select(-code) %>% 
  apply(., 2, function(x) x/sum(x)) %>% 
  cbind(cost = (1/GKP$cost)/sum(1/GKP$cost), .) %>% 
  apply(., 1, prod) -> probs

GKP %>% 
  select(name:cost, code) %>% 
  cbind(., probs) -> GKP_played

GKP <- list(played = GKP_played, fresh = GKP_fresh)
saveRDS(GKP, "GKP_pf.rds")

#defenders
data %>% 
  filter(pos == "DEF", status != "u") %>% 
  select(-one_of('status', 'threat')) -> DEF

DEF %>% 
  select(selected_by_percent:strength_defence_away) %>% 
  select(-code) %>% 
  apply(., 2, function(x) x/sum(x)) %>% 
  cbind(cost = (1/DEF$cost)/sum(1/DEF$cost), .) %>% 
  apply(., 1, prod) -> probs

DEF %>% 
  select(name:cost, code) %>% 
  cbind(., probs) -> DEF

saveRDS(DEF, "DEF.rds")

data %>% 
  filter(pos == "DEF", status != "u", total_points == 0) %>% 
  select(-one_of('status', 'threat')) -> DEF

DEF %>% 
  select(selected_by_percent, strength_overall_home:strength_defence_away) %>% 
  apply(., 2, function(x) x/sum(x)) %>% 
  cbind(cost = (1/DEF$cost)/sum(1/DEF$cost), .) %>% 
  apply(., 1, prod) -> probs

DEF %>% 
  select(name:cost, code) %>% 
  cbind(., probs) -> DEF_fresh


data %>% 
  filter(pos == "DEF", status != "u", total_points != 0) %>% 
  select(-one_of('status', 'threat')) -> DEF

DEF %>% 
  select(selected_by_percent:strength_defence_away) %>% 
  select(-code) %>% 
  apply(., 2, function(x) x/sum(x)) %>% 
  cbind(cost = (1/DEF$cost)/sum(1/DEF$cost), .) %>% 
  apply(., 1, prod) -> probs

DEF %>% 
  select(name:cost, code) %>% 
  cbind(., probs) -> DEF_played

DEF <- list(played = DEF_played, fresh = DEF_fresh)
saveRDS(DEF, "DEF_pf.rds")

#midfielders
data %>% 
  filter(pos == "MID", status != "u") %>% 
  select(-one_of('status')) -> MID

MID %>% 
  select(selected_by_percent:strength_defence_away) %>% 
  select(-code) %>% 
  apply(., 2, function(x) x/sum(x)) %>% 
  cbind(cost = (1/MID$cost)/sum(1/MID$cost), .) %>% 
  apply(., 1, prod) -> probs

MID %>% 
  select(name:cost, code) %>% 
  cbind(., probs) -> MID

saveRDS(MID, "MID.rds")

data %>% 
  filter(pos == "MID", status != "u", total_points == 0) %>% 
  select(-one_of('status')) -> MID

MID %>% 
  select(selected_by_percent, strength_overall_home:strength_defence_away) %>% 
  apply(., 2, function(x) x/sum(x)) %>% 
  cbind(cost = (1/MID$cost)/sum(1/MID$cost), .) %>% 
  apply(., 1, prod) -> probs

MID %>% 
  select(name:cost, code) %>% 
  cbind(., probs) -> MID_fresh

data %>% 
  filter(pos == "MID", status != "u", total_points != 0) %>% 
  select(-one_of('status')) -> MID

MID %>% 
  select(selected_by_percent:strength_defence_away) %>% 
  select(-code) %>% 
  apply(., 2, function(x) x/sum(x)) %>% 
  cbind(cost = (1/MID$cost)/sum(1/MID$cost), .) %>% 
  apply(., 1, prod) -> probs

MID %>% 
  select(name:cost, code) %>% 
  cbind(., probs) -> MID_played

MID <- list(played = MID_played, fresh = MID_fresh)
saveRDS(MID, "MID_pf.rds")

#forwards
data %>% 
  filter(pos == "FWD", status != "u") %>% 
  select(-one_of('status')) -> FWD

FWD %>% 
  select(selected_by_percent:strength_defence_away) %>% 
  select(-code) %>% 
  apply(., 2, function(x) x/sum(x)) %>% 
  cbind(cost = (1/FWD$cost)/sum(1/FWD$cost), .) %>% 
  apply(., 1, prod) -> probs

FWD %>% 
  select(name:cost, code) %>% 
  cbind(., probs) -> FWD

saveRDS(FWD, "FWD.rds")



data %>% 
  filter(pos == "FWD", status != "u", total_points == 0) %>% 
  select(-one_of('status')) -> FWD

FWD %>% 
  select(selected_by_percent, strength_overall_home:strength_defence_away) %>% 
  apply(., 2, function(x) x/sum(x)) %>% 
  cbind(cost = (1/FWD$cost)/sum(1/FWD$cost), .) %>% 
  apply(., 1, prod) -> probs

FWD %>% 
  select(name:cost, code) %>% 
  cbind(., probs) -> FWD_fresh


data %>% 
  filter(pos == "FWD", status != "u", total_points != 0) %>% 
  select(-one_of('status')) -> FWD

FWD %>% 
  select(selected_by_percent:strength_defence_away) %>% 
  select(-code) %>% 
  apply(., 2, function(x) x/sum(x)) %>% 
  cbind(cost = (1/FWD$cost)/sum(1/FWD$cost), .) %>% 
  apply(., 1, prod) -> probs

FWD %>% 
  select(name:cost, code) %>% 
  cbind(., probs) -> FWD_played

FWD <- list(played = FWD_played, fresh = FWD_fresh)
saveRDS(FWD, "FWD_pf.rds")

rm(GKP, DEF, MID, FWD, probs)
