#### Apply to predictions dataset
library(zoo)

rolling_average_predictions_team <- aleague_team_opp_data %>% 
  ungroup() %>% 
  group_by(team) %>% 
  dplyr::mutate(rolling.team.stats.corners_taken = rollapply(team.stats.corners_taken, list(-(0:9)), sum, fill=NA, align = "right", partial=F)/10,
                rolling.team.stats.corners_taken_against = rollapply(opp.stats.corners_taken, list(-(0:9)), sum, fill=NA, align = "right", partial=F)/10) %>% 
 
  summarise(rolling.team.stats.corners_taken = last(rolling.team.stats.corners_taken),
            rolling.team.stats.corners_taken_against = last(rolling.team.stats.corners_taken_against))


rolling_average_predictions_opp <- aleague_team_opp_data %>% 
dplyr::ungroup() %>% 
  group_by(opp.team) %>%               
  dplyr::mutate(rolling.opp.stats.corners_taken = rollapply(team.stats.corners_taken, list(-(0:9)), sum, fill=NA, align = "right", partial=F)/10,
                rolling.opp.stats.corners_taken_against = rollapply(opp.stats.corners_taken, list(-(0:9)), sum, fill=NA, align = "right", partial=F)/10) %>% 

  summarise(rolling.opp.stats.corners_taken = last(rolling.opp.stats.corners_taken),
            rolling.opp.stats.corners_taken_against = last(rolling.opp.stats.corners_taken_against))


aleague_predictions_data <- aleague_predictions_data %>% 
  dplyr::left_join(rolling_average_predictions_team, by = c("home.team" = "team")) %>% 
  dplyr::left_join(rolling_average_predictions_opp, by = c("away.team" = "opp.team")) 


#### team dataset

aleague_team_opp_data <- aleague_team_opp_data %>% 
  ungroup() %>% 
  group_by(team) %>% 
  dplyr::mutate(rolling.team.stats.corners_taken = rollapply(team.stats.corners_taken, list(-(0:9)), sum, fill=NA, align = "right", partial=F)/10,
                rolling.team.stats.corners_taken_against = rollapply(opp.stats.corners_taken, list(-(0:9)), sum, fill=NA, align = "right", partial=F)/10) %>% 
  
  dplyr::ungroup() %>% 
  group_by(opp.team) %>%               
  dplyr::mutate(rolling.opp.stats.corners_taken = rollapply(team.stats.corners_taken, list(-(0:9)), sum, fill=NA, align = "right", partial=F)/10,
                rolling.opp.stats.corners_taken_against = rollapply(opp.stats.corners_taken, list(-(0:9)), sum, fill=NA, align = "right", partial=F)/10)
                
                

