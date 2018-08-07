rm(list = ls())
library(tidyverse)
library(magrittr)
library(fplr)

df <- fpl_get_fixtures()

btsrp <- fpl_get_bootstrap()
names(btsrp)

btsrp$elements
btsrp$stats


fpl_get_teams()

plrs <- fpl_get_players()

fpl_get_user_performance(2828585)

teams <- fpl_get_teams()
