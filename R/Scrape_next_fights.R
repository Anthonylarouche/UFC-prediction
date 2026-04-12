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

pred<-dbGetQuery(con, 'SELECT * FROM Next_fight_prediction')

fight<-dbGetQuery(con, 'SELECT * FROM Fight_data')

key_cols <- c("Fighter1", "Fighter2", "Date")
new_rows <- next_fight_new %>%
  anti_join(pred, by = key_cols)

if(!any(next_fight$Date %in% pred$Date)){
  dbWriteTable(con, "Next_fight_prediction", next_fight_new, append = TRUE)
  pred_result_updated <- pred %>%
    left_join(
      fight %>% select(fighter_1_fighter, dates, fighter_1_res),
      by = c("Fighter1" = "fighter_1_fighter", "Date" = "dates")
    ) %>%
    mutate(
      fighter_1_res = coalesce(fighter_1_res.y, fighter_1_res.x)
    ) %>%
    select(-fighter_1_res.x, -fighter_1_res.y)
  
  dbWriteTable(con, "Next_fight_prediction", pred_result_updated, overwrite = TRUE)
}
if(nrow(new_rows)>0){
  dbWriteTable(con, "Next_fight_prediction", new_rows, append = TRUE)

  pred_result_updated <- pred %>%
    left_join(
      fight %>% select(fighter_1_fighter, dates, fighter_1_res),
      by = c("Fighter1" = "fighter_1_fighter", "Date" = "dates")
    ) %>%
    mutate(
      fighter_1_res = coalesce(fighter_1_res.y, fighter_1_res.x)
    ) %>%
    select(-fighter_1_res.x, -fighter_1_res.y)
  
  dbWriteTable(con, "Next_fight_prediction", pred_result_updated, overwrite = TRUE)
}else{
  message("Table already updated")
}

}



# pred |>
#   mutate(Date = as.Date(Date, format = "%B %d, %Y")) |>
#   group_by(Date) |>
#   filter(!is.na(fighter_1_res), fighter_1_res != "D") |>
#   summarise(
#     accuracy = mean(
#       (prob_win_fighter1 > 0.5) == (fighter_1_res == "W")
#     )
#   ) |>
#   arrange(Date)
# 
# 
# 
# pred |>
#   filter(!is.na(fighter_1_res), fighter_1_res != "D") |>
#   summarise(
#     accuracy = mean(
#       (prob_win_fighter1 > 0.5) == (fighter_1_res == "W")
#     )
#   )
# 
# cor(
#   pred$prob_win_fighter1,
#   pred$fighter_1_res == "W",
#   use = "complete.obs"
# )
# 
# 
# pred |>
#   filter(!is.na(fighter_1_res), fighter_1_res != "D")|>
#   mutate(bin = cut(prob_win_fighter1, breaks = seq(0,1,0.05))) |>
#   group_by(bin) |>
#   summarise(
#     n = n(),
#     win_rate = mean(fighter_1_res == "W")
#   )
# 
# plot(test$bin,test$win_rate)
