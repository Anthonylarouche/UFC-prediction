updated_fighters<-function(con) {

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

fighter_updated_record <- map2_dfr(fighters$link, fighters$name, function(link, name){

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
        record = record,
        ID = str_replace(link,"http://www.ufcstats.com/fighter-details/","")
      )
    
    return(info_df)
    
  }, error = function(e) {
    
    return(tibble(
      name = name,
      link = link
    ))
    
  })
  
})

new_fighters <-all_links|>
  anti_join(fighters , by = "link") 

dbWriteTable(con, "Fighters", fighter_updated_record, overwrite = TRUE)

if(nrow(new_fighters)!= 0){

message(paste0("\nNew fighter : \n",new_fighters$name))
  
}else{
  message("No fighter to scrapped")
 }

}