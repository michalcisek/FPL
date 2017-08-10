rm(list = ls())

source("load_packages.R")
source("sample_position.R")
source("check_position_limit.R")
source("check_team_limit.R")
source("check_team_value.R")
source("sample_player_GKP.R")
source("sample_player_DEF.R")
source("sample_player_MID.R")
source("sample_player_FWD.R")
source("sample_team.R")

#number of simulations
nsim <- 1000
#type of simulation - only_played or inlude_fresh
type <- "include_fresh"
#sample approach of selecting team?
sample_approach <- F

#load data
if(type == "include_fresh"){
  data <- readRDS("data.rds")
  gkp <- readRDS("GKP_pf.rds")
  def <- readRDS("DEF_pf.rds")
  mid <- readRDS("MID_pf.rds")
  fwd <- readRDS("FWD_pf.rds")
} else if(type == "only_played"){
  data <- readRDS("data.rds")
  gkp <- readRDS("GKP.rds")
  def <- readRDS("DEF.rds")
  mid <- readRDS("MID.rds")
  fwd <- readRDS("FWD.rds")
}

# Run simulation ----------------------------------------------------------
if(sample_approach == TRUE){
  simulation <- vector("list", nsim)
  for(i in 1:nsim){
    approach <- sample(c("balanced", "midfield", "forward"), 1, prob = rep(1/3, 3))
    tolerance <- sample(seq(1, 1.2, by = 0.01), 1, prob = rep(1/21, 21))
    
    simulation[[i]] <- sample_team(data, gkp, def, mid, fwd, type = type, approach, tolerance)
    print(i)
    flush.console()
  }
} else {
  simulation <- pbsapply(1:nsim, function(x) sample_team(data, gkp, def, mid, fwd, 
                                                        type = type, approach = "midfield", tolerance = 1))
}

simulation %>% 
  unlist %>% 
  table %>% 
  c %>% 
  data.frame(code = names(.), count = .) -> simulation

simulation <- sqldf::sqldf("select a.*, b.name, b.cost, b.pos, b.team from simulation a left join
                            data b on a.code=b.code")



# Linear programming to select most valuable players ----------------------
#objective function
f.obj <- simulation$count

#constraints
con_cost <- simulation$cost
con_players <- rep(1, nrow(simulation))
con_gkp <- ifelse(simulation$pos == "GKP", 1, 0)
con_def <- ifelse(simulation$pos == "DEF", 1, 0)
con_mid <- ifelse(simulation$pos == "MID", 1, 0)
con_fwd <- ifelse(simulation$pos == "FWD", 1, 0)
con_team <- sapply(levels(simulation$team), function(x) ifelse(simulation$team == x, 1, 0)) %>% t

f.con <- matrix(c(con_cost, con_players, con_gkp, con_def, con_mid, con_fwd, con_team), 
                nrow = (6 + nrow(con_team)), byrow = TRUE)
f.dir <- c("<=", "==", "==", "==", "==", "==", rep("<=", nrow(con_team)))
f.rhs <- c(100, 15, 2, 5, 5, 3, rep(3, nrow(con_team)))

lp <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)

#selected team
selected <- simulation[which(lp$solution == 1), ]

#check
table(selected$pos)
table(selected$team)
sum(selected$cost)

#example of one simulation
# t <- sample_team(data, gkp, def, mid, fwd, type = "only_played", approach = "balanced", tolerance = 1)