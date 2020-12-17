#### Goals ####

poisson_model_goals <- 
  rbind(
    data.frame(goals=aleague_match_data$home.stats.goals,
               team=aleague_match_data$home.team,
               opponent=aleague_match_data$away.team,
               #weather=as.character(aleague_match_data$month),
               venue=aleague_match_data$venue,
               dayornight=aleague_match_data$day.night,
               round = aleague_match_data$round_band,
               weight = aleague_match_data$date.weight,
               season = as.factor(aleague_match_data$season),
               home.advantage.cat= aleague_match_data$home.advantage.cat),
    data.frame(goals=aleague_match_data$away.stats.goals,
               team=aleague_match_data$away.team,
               opponent=aleague_match_data$home.team,
               #weather=as.character(aleague_match_data$month),
               venue=aleague_match_data$venue,
               dayornight=aleague_match_data$day.night,
               round = aleague_match_data$round_band,
               weight = aleague_match_data$date.weight,
               season = as.factor(aleague_match_data$season),
               home.advantage.cat = -aleague_match_data$home.advantage.cat)) %>% 
  glm(goals ~ home.advantage.cat + team + opponent 
      #+ weather 
      + dayornight + round, family=poisson(link=log),data=.
      , weights=(weight))

summary(poisson_model_goals)


#### Corners ####
poisson_model_corners <- 
  rbind(
    data.frame(corners=aleague_match_data$home.stats.corners_taken,
               team=aleague_match_data$home.team,
               opponent=aleague_match_data$away.team,
               
               #weather=as.character(aleague_match_data$month),
               dayornight=aleague_match_data$day.night,
               round = aleague_match_data$round_band,
               weight = aleague_match_data$date.weight,
               season = as.factor(aleague_match_data$season),
               home.advantage.cat = aleague_match_data$home.advantage.cat),
    data.frame(corners=aleague_match_data$away.stats.corners_taken,
               team=aleague_match_data$away.team,
               opponent=aleague_match_data$home.team,
               #weather=as.character(aleague_match_data$month),
               dayornight=aleague_match_data$day.night,
               round = aleague_match_data$round_band,
               weight = aleague_match_data$date.weight,
               season = as.factor(aleague_match_data$season),
               home.advantage.cat = -aleague_match_data$home.advantage.cat)) %>% 
  glm(corners ~ home.advantage.cat + team + opponent 
      #+ weather 
      + dayornight + round, family=poisson(link=log),data=.
      , weights=(weight))

summary(poisson_model_corners)



#### Predictions ####

aleague_predictions_data$home.goals.poisson <- predict(poisson_model_goals, 
                                                       data.frame(team=aleague_predictions_data$home.team,
                                                                  opponent=aleague_predictions_data$away.team,
                                                                  #weather=as.character(aleague_predictions_data$month),
                                                                  dayornight=aleague_predictions_data$day.night,
                                                                  round = aleague_predictions_data$round_band,
                                                                  season = as.factor(aleague_predictions_data$season),
                                                                  home.advantage.cat = aleague_predictions_data$home.advantage.cat), type="response")

aleague_predictions_data$away.goals.poisson <- predict(poisson_model_goals, 
                                                       data.frame(team=aleague_predictions_data$away.team,
                                                                  opponent=aleague_predictions_data$home.team,
                                                                  #weather=as.character(aleague_predictions_data$month),
                                                                  dayornight=aleague_predictions_data$day.night,
                                                                  round = aleague_predictions_data$round_band,
                                                                  season = as.factor(aleague_predictions_data$season),
                                                                  home.advantage.cat = aleague_predictions_data$home.advantage.cat), type="response")

#### Total Goals & Goals Band
aleague_predictions_data$total.goals.poisson <- aleague_predictions_data$home.goals.poisson + aleague_predictions_data$away.goals.poisson

aleague_predictions_data <- aleague_predictions_data %>% 
  dplyr::mutate(total.goals_band.poisson = case_when(total.goals.poisson <= 2  ~ '0-2',
                                                     total.goals.poisson > 2 & total.goals.poisson <= 4 ~ '3-4',
                                                     total.goals.poisson > 4  ~ '4+'))

#### Corners Predictions
aleague_predictions_data$home.corners.poisson <- predict(poisson_model_corners, 
                                              data.frame(team=aleague_predictions_data$home.team,
                                                         opponent=aleague_predictions_data$away.team,
                                                         #weather=as.character(aleague_predictions_data$month),
                                                         dayornight=aleague_predictions_data$day.night,
                                                         round = aleague_predictions_data$round_band,
                                                         season = as.factor(aleague_predictions_data$season),
                                                         home.advantage.cat = aleague_predictions_data$home.advantage.cat), type="response")

aleague_predictions_data$away.corners.poisson <- predict(poisson_model_corners, 
                                              data.frame( team=aleague_predictions_data$away.team,
                                                         opponent=aleague_predictions_data$home.team,
                                                         #weather=as.character(aleague_predictions_data$month),
                                                         dayornight=aleague_predictions_data$day.night,
                                                         round = aleague_predictions_data$round_band,
                                                         season = as.factor(aleague_predictions_data$season),
                                                         home.advantage.cat = aleague_predictions_data$home.advantage.cat), type="response")

aleague_predictions_data$total.corners.poisson <- aleague_predictions_data$home.corners.poisson + aleague_predictions_data$away.corners.poisson

aleague_predictions_data <- aleague_predictions_data %>% 
  dplyr::mutate(total.corners_band.poisson = case_when(total.corners.poisson <= 9  ~ '0-2',
                                                     total.corners.poisson > 9 & total.corners.poisson <= 12 ~ '3-4',
                                                     total.corners.poisson > 12  ~ '4+'))




#### Trailing vairable
aleague_predictions_data <- aleague_predictions_data %>% 
  mutate(home.est_mins_trailing = case_when(home.goals.poisson-away.goals.poisson == -1 ~ 30,
                                          home.goals.poisson-away.goals.poisson <= -2 ~ 45,
                                          TRUE ~ 0),
       away.est_mins_trailing = case_when(away.goals.poisson-home.goals.poisson == -1 ~ 30,
                                         away.goals.poisson-home.goals.poisson<= -2 ~ 45,
                                         TRUE ~ 0))

