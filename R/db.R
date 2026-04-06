library(DBI)
library(RSQLite)

connect_db <- function() {
  dbConnect(RSQLite::SQLite(), "~/Documents/UFC_prediction/Data/UFC_project.db")
}

disconnect_db <- function(con) {
  dbDisconnect(con)
}


