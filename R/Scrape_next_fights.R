updated_nextfights<-function(new_elo,con){

UFC_next <- tibble(UFC_Page = "http://ufcstats.com/statistics/events/completed?page=all") %>% 
  # Scrape Fight Card links
  mutate(cards = map(UFC_Page, scrape_next_cards)) %>% 
  unnest(cards) %>%
  mutate(dates = scrape_dates(UFC_Page)[1] )


next_fight<-read_html(UFC_next$cards)%>%
  html_nodes("p") %>% 
  html_text2()

next_fight <- next_fight |>
  matrix(ncol = 8, byrow = TRUE) |>
  as_tibble() |>
  dplyr::select(1, 2, 4)

next_fight$date <- UFC_next$dates
colnames(next_fight)<-c("Fighter1","Fighter2","category","Date")

prob_output<-elo.prob(ifelse(is.na(new_elo[next_fight$Fighter1]),1500,new_elo[next_fight$Fighter1]),
                      ifelse(is.na( new_elo[next_fight$Fighter2]),1500,new_elo[next_fight$Fighter2]))

next_fight_new<-next_fight%>%
  mutate(prob_win_fighter1 = prob_output)


toJSON(next_fight_new, pretty = TRUE)|>
  write( file = "Data/ufc_predictions.json")

Date<-dbGetQuery(con, 'SELECT Date FROM Next_figth_prediction')
fight<-dbGetQuery(con, 'SELECT * FROM Fight_data')

if(!any(next_fight$Date %in% Date$Date)){
  dbWriteTable(con, "Next_figth_prediction", next_fight_new, append = TRUE)
  pred<-dbGetQuery(con, 'SELECT * FROM Next_figth_prediction')
  
  pred_result_updated <- pred %>%
    left_join(
      fight %>% select(fighter_1_fighter, dates, fighter_1_res),
      by = c("Fighter1" = "fighter_1_fighter", "Date" = "dates")
    ) %>%
    mutate(
      fighter_1_res = coalesce(fighter_1_res.y, fighter_1_res.x)
    ) %>%
    select(-fighter_1_res.x, -fighter_1_res.y)
  
  dbWriteTable(con, "Next_figth_prediction", pred_result_updated, overwrite = TRUE)
}else{
  message("Table already updated")
}

}


# pred|>
#   group_by(Date)|>
#   summarise(accuracy = sum(fighter_1_res == "W" & prob_win_fighter1>0.5,na.rm = T)/
#               sum(fighter_1_res == "W" | fighter_1_res == "L" ,na.rm = T))
