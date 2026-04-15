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

# result<-elo.run(Result ~ Fighter_1 + Fighter_2, data = df_elo, k=30)|>
#   final.elos()

update_elo <- function(rA, rB, outcome, k = 30, s = 235) {
  pA <- 1 / (1 + 10^(-(rA - rB)/s))
  
  rA_new <- rA + k * (outcome - pA)
  rB_new <- rB + k * ((1 - outcome) - (1 - pA))
  
  list(rA = rA_new, rB = rB_new)
}


`%||%` <- function(a, b) if (!is.null(a)) a else b


# état courant
ratings <- list()

df <- df_elo %>%
  mutate(date = as.Date(date))%>%
  arrange(date, fight_pk)


# historique
df <- df %>%
  mutate(
    eloA_before = NA_real_,
    eloB_before = NA_real_,
    probA_win = NA_real_,
    probB_win = NA_real_,
    eloA_after  = NA_real_,
    eloB_after  = NA_real_
  )


for (i in seq_len(nrow(df))) {
  
  fA <- df$Fighter_1[i]
  fB <- df$Fighter_2[i]
  
  rA <- ratings[[fA]] %||% 1500
  rB <- ratings[[fB]] %||% 1500
  
  df$eloA_before[i] <- rA
  df$eloB_before[i] <- rB
  
  df$probA_win[i]<-prob.elo(rA,rB,235)[[1]]
  df$probB_win[i]<-prob.elo(rA,rB,235)[[2]]
  
  res <- update_elo(rA, rB, df$Result[i])
  
  df$eloA_after[i] <- res$rA
  df$eloB_after[i] <- res$rB
  
  ratings[[fA]] <- res$rA
  ratings[[fB]] <- res$rB
}

return(ratings)

}


