library(rvest)
library(httr)
library(dplyr)
library(tidyselect)
library(lubridate)
library(RPostgres)
library(tidyselect)
library(lubridate)
library(tidyr)
library(elo)
library(purrr)
library(broom)
library(jsonlite)

source("~/Documents/UFC_prediction/R/functions.R")
source("~/Documents/UFC_prediction/R/db.R")
source("~/Documents/UFC_prediction/R/Scrape_fighters.R")
source("~/Documents/UFC_prediction/R/Scrape_fights.R")
source("~/Documents/UFC_prediction/R/Elo.R")
source("~/Documents/UFC_prediction/R/Scrape_next_fights.R")


con<-connect_db()


# Scrape les combattants
updated_fighters(con)

# Scrape les combats
updated_fights(con)

# Update le Elo
new_elo<-updated_elo(con)
saveRDS(new_elo, "../Model/Elo_model.rds")

# Scrape next fight + ajouter prédictions + fichier JSON 
updated_nextfights(new_elo,con)

disconnect_db(con)

