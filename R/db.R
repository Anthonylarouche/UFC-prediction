
connect_db <- function() {
  dbConnect(RSQLite::SQLite(), "Data/UFC_project.db")
}

disconnect_db <- function(con) {
  dbDisconnect(con)
}


