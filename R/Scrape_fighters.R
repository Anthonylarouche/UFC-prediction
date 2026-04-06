updated_fighters<-function(con){
  
fighters<-dbGetQuery(con, 'SELECT * FROM Fighters')

all_links <- map_dfr(letters, \(i) {
  
  url <- paste0("http://www.ufcstats.com/statistics/fighters?char=", i, "&page=all")
  page <- read_html(url)
  
  # Récupère tous les liens
  page %>%
    html_nodes(".b-link_style_black") %>%
    { tibble(
      name = html_text(., trim = TRUE),
      link = html_attr(., "href")
    )
    }
  
}) %>%
  group_by(link) %>%  
  slice(1:2)%>%
  summarise(
    name = paste(name, collapse = " "),  
    .groups = "drop"
  )

fighter_to_scrapped <-all_links|>
  anti_join(fighters %>% dplyr::select(link) %>% distinct(), by = "link") 

if(nrow(fighter_to_scrapped)!= 0){
  
  fighter_stats <- map2_dfr(fighter_to_scrapped$link, fighter_to_scrapped$name, function(link, name) {
    
    tryCatch({
      
      page <- read_html(link)
      
      info <- page %>%
        html_nodes(".b-list__box-list") %>%
        .[[1]] %>%
        html_nodes(".b-list__box-list-item") %>%
        html_text(trim = TRUE) %>%
        str_replace_all("\n", "") %>%
        str_squish()
      
      record <- page %>%
        html_nodes(".b-content__title-record")%>%
        html_text(trim = TRUE) %>%
        str_replace_all("\n|Record:", "") %>%
        str_squish()
      
      info_df <- tibble(raw = info) %>%
        mutate(
          label = str_extract(raw, "^[^:]+"),
          value = str_extract(raw, "(?<=: ).*")
        ) %>%
        select(label, value) %>%
        pivot_wider(names_from = label, values_from = value) %>%
        mutate(
          name = name,
          link = link,
          record = record
        )
      
      return(info_df)
      
    }, error = function(e) {
      
      return(tibble(
        name = name,
        link = link
      ))
      
    })
    
  })

dbWriteTable(con, "Fighters", fighter_stats, append = TRUE)
  
}else{
  message("No fighter to scrapped")
 }
}