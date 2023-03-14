.initial_cleaning <- function(boxscores){

    team_off_data = boxscores %>% 
      select(team_id,team_display_name,
             game_id,game_date,
             field_goals_made_field_goals_attempted,three_point_field_goals_made_three_point_field_goals_attempted,
             free_throws_made_free_throws_attempted,offensive_rebounds,defensive_rebounds,
             assists,steals,blocks,turnovers,team_turnovers,total_turnovers,technical_fouls,
             flagrant_fouls,fouls,largest_lead,
             home_away,
             opponent_id) %>% 
      arrange(desc(game_date)) %>% # Fix home_away
      mutate(home_away = ifelse(home_away=="HOME","AWAY","HOME")) %>% 
      separate(col=field_goals_made_field_goals_attempted,
               into = c("FGM","FGA"),sep="-") %>% 
      separate(col=three_point_field_goals_made_three_point_field_goals_attempted,
               into = c("FG3M","FG3A"),sep="-") %>% 
      separate(col=free_throws_made_free_throws_attempted,
               into = c("FTM","FTA"),sep="-") %>% 
      mutate(FGM = as.numeric(FGM),
             FGA = as.numeric(FGA),
             FTM = as.numeric(FTM),
             FTA = as.numeric(FTA),
             FG3M = as.numeric(FG3M),
             FG3A = as.numeric(FG3A)) %>% 
      mutate(PTS = 2 * FGM + 1 * FG3M + 1 * FTM) %>%
      # Cleanup
        mutate(
            offensive_rebounds = as.numeric(offensive_rebounds),
            defensive_rebounds = as.numeric(defensive_rebounds),
            assists = as.numeric(assists),
            steals = as.numeric(steals),
            blocks = as.numeric(blocks),
            turnovers = as.numeric(turnovers),
            team_turnovers = as.numeric(team_turnovers),
            total_turnovers = as.numeric(total_turnovers),
            technical_fouls = as.numeric(technical_fouls),
            flagrant_fouls = as.numeric(flagrant_fouls),
            fouls = as.numeric(fouls),
            largest_lead = as.numeric(largest_lead),
            home_away = as.factor(home_away),
            opponent_id = as.character(opponent_id)
        
        )
    all_team_data = team_off_data %>% 
      left_join(team_off_data,by=c("opponent_id"="team_id","game_id","game_date")) %>% 
      mutate(winning_margin = PTS.x - PTS.y,
             game_total = PTS.x + PTS.y) %>% 
      mutate(game_date = as.Date(game_date)) %>% 
      mutate(SEASON = ifelse(month(as.POSIXlt(game_date, format="Y-m-d"))>5,
                             year(as.POSIXlt(game_date, format="Y-m-d"))+1,
                             year(as.POSIXlt(game_date, format="Y-m-d")))) %>% 
      # Identify appearance number
      group_by(SEASON,team_id) %>% 
      arrange(SEASON,team_id,game_date) %>% 
      mutate(APPEARANCE = row_number()) %>% 
      ungroup()

    all_team_data = all_team_data %>% 
      left_join(all_team_data %>% 
                  select(game_id,team_id,APPEARANCE) %>% 
                  rename("OPP_APPEARANCE"="APPEARANCE"),by=c("opponent_id"="team_id","game_id"))
    
    return(all_team_data)
    
}

initial_cleaning = memoise::memoise(.initial_cleaning)