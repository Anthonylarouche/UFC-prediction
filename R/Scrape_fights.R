
updated_fights<-function(con){
  scraped_cards<-dbGetQuery(con, 'SELECT * FROM scrapped_card')
    
    UFC <- tibble(UFC_Page = "http://ufcstats.com/statistics/events/completed?page=all") %>% 

            mutate(cards = map(UFC_Page, scrape_cards)) %>% 
      unnest(cards)%>%
      mutate(dates = scrape_dates("http://ufcstats.com/statistics/events/completed?page=all")[-1])%>%
      anti_join(scraped_cards %>% dplyr::select(cards) %>% distinct(), by = "cards") 
    
    if(nrow(UFC) != 0){
      UFC <- UFC  %>% 
    
        mutate(fights = map(cards, scrape_fights)) %>% unnest(fights)
      UFC_Data <- UFC %>% mutate(fight_data = map(fights, safely(scrape_fight_summary_data)))
      df <- UFC_Data %>% 
        unnest(fight_data) %>% 
        filter(row_number() %% 2 != 0) %>% 
        unnest(fight_data)  %>% 
        distinct()
      
      df_raw<- bind_rows(df,scraped_cards|>
                           select(!contains("fight_pk")))%>% 
        mutate(row_num = row_number()) %>% 
        arrange(desc(row_num)) %>% 
        mutate(fight_pk = row_number()) %>% 
        arrange(row_num) %>% 
        dplyr::select(-row_num) 
      
    }
    
    ######### Obtenir une version complète 1 ligne = 1 combat #################
    
    if(nrow(UFC) != 0){
      df_clean<-df_raw %>%
        dplyr::select(-UFC_Page, -cards, -fights) %>%
        rename_all(tolower) %>%
        mutate(
          date = as.Date(dates, format = "%B %d, %Y"),
          fighter_1_fighter = as.factor(fighter_1_fighter),
          fighter_2_fighter = as.factor(fighter_2_fighter),
          method = as.factor(method),
          fighter_1_res = as.factor(fighter_1_res),
          fighter_2_res = as.factor(fighter_2_res),
          weight_class = as.factor(weight_class)
        ) %>%
        mutate(across(contains("percent"), ~ as.numeric(str_replace(.x, "%", "")) * 0.01)) %>%
        mutate(across(contains("landed", ignore.case = FALSE), as.numeric)) %>%
        mutate(across(contains("attempts", ignore.case = FALSE), as.numeric))%>%
        mutate(across(contains("rev", ignore.case = FALSE), as.numeric))%>%
        mutate(across(contains("kd", ignore.case = FALSE), as.numeric))%>%
        mutate(across(contains("ctrl", ignore.case = TRUE), ~ {
          as.numeric(ms(.x)) 
        }))%>%
        mutate(fighter_1_sig_strike_percent = fighter_1_sig_strike_landed / fighter_1_sig_strike_attempts,
               fighter_1_td_percent = fighter_1_td_landed / fighter_1_td_attempts,
               fighter_2_sig_strike_percent = fighter_2_sig_strike_landed / fighter_2_sig_strike_attempts,
               fighter_2_td_percent = fighter_2_td_landed / fighter_2_td_attempts) %>% 
        # If no attempts than percent will be 0 (may be adjusted to a not attempted variable)
        mutate_at(vars(contains("percent")), .funs = ~if_else(is.nan(.x) == TRUE, 0, .x)) %>% 
        # Convert rounds and round_finished
        mutate(time_format = str_extract(time_format, "\\d")) %>% 
        rename("rounds" = time_format) %>% 
        mutate(rounds  = as.factor(rounds),
               round_finished = as.factor(round_finished)) %>% 
        # Convert method
        separate(method, into = c("method"), sep = "-", extra = "drop") %>% 
        mutate(method = str_trim(method),
               method = as.factor(method)) %>% 
        # Convert weight_class
        mutate(gender = str_extract(weight_class, "Women")) %>% 
        mutate(gender = if_else(is.na(gender) == TRUE, "Men", gender)) %>% 
        mutate(weight_class = str_extract(weight_class, "\\w+weight")) %>% 
        mutate(weight_class = if_else(is.na(weight_class == TRUE), "Catchweight", weight_class)) %>% 
        unite("weight_class", c(gender, weight_class)) %>% 
        mutate(weight_class = as.factor(weight_class)) 
      
dbWriteTable(con,"scrapped_card", df_raw,row.names = FALSE,overwrite = TRUE)
dbWriteTable(con,"Fight_data", df_clean,row.names = FALSE,overwrite = TRUE)  

    }else{
      message("Aucune données à nettoyer")
    }    
}

