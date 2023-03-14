calculate_relative_elo <- function(team_data_refined,
                                   features,
                                   season,
                                   rename_fs = ""){
  
  columns = c("game_date","game_id","team_id","opponent_id","APPEARANCE","OPP_APPEARANCE",
              feature)
  
  temp = team_data_refined %>% 
    select(columns) %>% 
    arrange(APPEARANCE) %>% 
    group_by(APPEARANCE) %>% 
    mutate(F_AVG = mean(!!as.name(feature)),
           F_SD = sd(!!as.name(feature))) %>% 
    ungroup() %>% 
    mutate(
      PERF = (!!as.name(feature) - F_AVG)/F_SD,
    )
  
  if(rename_f!=""){
    feature = rename_f
  }
  
  temp = temp %>% 
    mutate("{feature}_ELO" := 2000,
           "{feature}_DEF_ELO" := 2000, # Corresponds to opponent
           "{feature}_ELO_NEXT" := 2000,
           "{feature}_DEF_ELO_NEXT" := 2000) %>% 
    arrange(game_date)
  
  for(r in 1:nrow(temp)){
    
    row = temp[r,]
    team = row$team_id
    opp = row$opponent_id
    
    if(row$APPEARANCE!=1){
      # Set row {feature}_ELO to {feature}_ELO_NEXT from prior appearance
      t_pa = temp %>% 
        filter(APPEARANCE == row$APPEARANCE - 1,
               team_id == team)
      row[[paste0(feature,"_ELO")]] = t_pa[[paste0(feature,"_ELO_NEXT")]]
    }
    if(row$OPP_APPEARANCE!=1){
      # Set row {defending_feature}_ELO to {defending_feature}_ELO_NEXT from prior appearance by opponent
      o_pa = temp %>% 
        filter(APPEARANCE == row$OPP_APPEARANCE - 1,
               team_id == opp)
      row[[paste0(feature,"_DEF_ELO")]] = o_pa[[paste0(feature,"_DEF_ELO_NEXT")]]
    }
    
    perf = ifelse(is.nan(row$PERF),0,row$PERF)

    # Negative should hurt feature, benefit d_feature
    # Weight of benefit should be impacted by pregame opinion on strengths
    
    w_prob = row[[paste0(feature,"_ELO")]] / (row[[paste0(feature,"_ELO")]] + 
                                                row[[paste0(feature,"_DEF_ELO")]])
    
    # W_prob > 0.5 means team should have advantage... smaller change if positive perf
    f_elo_change = ifelse(w_prob >= 0.5, 2*(w_prob-0.5)**2+0.5 , 1 - 2*(w_prob-0.5)**2+0.5) * 100 * perf 
    

    row[[paste0(feature,"_ELO_NEXT")]] =  row[[paste0(feature,"_ELO")]] + f_elo_change
    row[[paste0(feature,"_DEF_ELO_NEXT")]] = row[[paste0(feature,"_DEF_ELO")]] - f_elo_change
    
    temp[r,] = row
    print('Done')
    
  }
  return(temp)
  
}


BOOSTED_calculate_relative_elo <- function(team_data_refined,
                                   features,
                                   rename_fs = ""){
  
    columns = c("game_date","game_id","team_id","opponent_id","APPEARANCE","OPP_APPEARANCE",
              features)
  
    # Group By stays the same for all features
    temp = team_data_refined %>% 
        select(columns) %>% 
        arrange(APPEARANCE) %>% 
        group_by(APPEARANCE)
    
    # Calculate PERF for features
    for(i in 1:length(features)){
        feature = features[i]
        fs = rename_fs[i]
        temp = temp %>%
            mutate("{fs}_AVG" := mean(!!as.name(feature)),
                   "{fs}_SD" := mean(!!as.name(feature))) %>%
            mutate("{fs}_PERF" := (!!as.name(feature) - !!as.name(paste0(fs,"_AVG")))/!!as.name(paste0(fs,"_SD")),
                   "{fs}_ELO" := 2000,
                   "{fs}_DEF_ELO" := 2000, # Corresponds to opponent
                   "{fs}_ELO_NEXT" := 2000,
                   "{fs}_DEF_ELO_NEXT" := 2000)
    }
    temp = temp %>%
        ungroup() %>%
        arrange(game_date)
  
    for(r in 1:nrow(temp)){
    
        row = temp[r,]
        team = row$team_id
        opp = row$opponent_id
    
        if(row$APPEARANCE!=1){
            # Run for all features
            t_pa = temp %>% 
                filter(APPEARANCE == row$APPEARANCE - 1,
                       team_id == team)
            # Set row {feature}_ELO to {feature}_ELO_NEXT from prior appearance FOR ALL FEATURES
            for(feature in rename_fs){
                row[[paste0(feature,"_ELO")]] = t_pa[[paste0(feature,"_ELO_NEXT")]]
            }
        }
        if(row$OPP_APPEARANCE!=1){
              # Set row {defending_feature}_ELO to {defending_feature}_ELO_NEXT from prior appearance by opponent FOR ALL FEATURES
              o_pa = temp %>% 
                  filter(APPEARANCE == row$OPP_APPEARANCE - 1,
                  team_id == opp)
              for(feature in rename_fs){
                  print(feature)
                  row[[paste0(feature,"_DEF_ELO")]] = o_pa[[paste0(feature,"_DEF_ELO_NEXT")]]
              }
          }
        
        # Calculate perf for ALL FEATURES
        for(feature in rename_fs){
            perf = ifelse(is.nan(row[[paste0(fs,'_PERF')]]),0,row[[paste0(fs,'_PERF')]])
            # Negative should hurt feature, benefit d_feature
            # Weight of benefit should be impacted by pregame opinion on strengths
    
            w_prob = row[[paste0(feature,"_ELO")]] / (row[[paste0(feature,"_ELO")]] + 
                                                row[[paste0(feature,"_DEF_ELO")]])
            
            # W_prob > 0.5 means team should have advantage... smaller change if positive perf
            f_elo_change = ifelse(w_prob >= 0.5, 2*(w_prob-0.5)**2+0.5 , 1 - 2*(w_prob-0.5)**2+0.5) * 100 * perf 
            row[[paste0(feature,"_ELO_NEXT")]] =  row[[paste0(feature,"_ELO")]] + f_elo_change
            row[[paste0(feature,"_DEF_ELO_NEXT")]] = row[[paste0(feature,"_DEF_ELO")]] - f_elo_change
        }
        
        temp[r,] = row
    
  }
  return(temp)
  
}
