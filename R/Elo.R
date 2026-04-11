updated_elo<-function(con){
df_fight_data<-dbGetQuery(con, 'SELECT * FROM Fight_data')%>%
  select(date,fight_pk,fighter_1_fighter,fighter_1_res,fighter_2_fighter,weight_class)

colnames(df_fight_data)<-c("date","fight_pk","Fighter_1","Result","Fighter_2","Weight_class")

df_elo<-df_fight_data%>%
  mutate(Result = case_when(
    Result == "W" ~ 1,
    Result == "L" ~ 0,
    .default = 0.5
  ))%>%
  ungroup()%>%
  arrange(fight_pk)

result<-elo.run(Result ~ Fighter_1 + Fighter_2, data = df_elo, k=30)|>
  final.elos()
return(result)

}


