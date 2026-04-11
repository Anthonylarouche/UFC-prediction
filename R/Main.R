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
library(DBI)
library(RSQLite)

source("R/functions.R")
source("R/db.R")
source("R/Scrape_fighters.R")
source("R/Scrape_fights.R")
source("R/Elo.R")
source("R/Scrape_next_fights.R")


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

