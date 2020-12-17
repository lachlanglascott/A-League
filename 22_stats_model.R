#### Ball in Box Possession Rating ####

attacking_possession_rating <- 

  lm(team.stats.ball_in_box ~
       team.stats.possessions +
       opp.stats.possessions +
       team.stats.runs +
       team.stats.backward_passes +
       team.stats.long_passes +
       team.stats.sideway_passes +
       team.stats.passes_regained,
     data = aleague_team_opp_data)

summary(attacking_possession_rating)

aleague_team_opp_data$team.attacking_possession_rating <- predict(attacking_possession_rating, aleague_team_opp_data)
aleague_team_opp_data$team.attacking_possession_rating <- aleague_team_opp_data$team.attacking_possession_rating/mean(aleague_team_opp_data$team.attacking_possession_rating, na.rm = TRUE)


#### Ball in Box Poressure Rating ####

attacking_pressure_rating <- 
  
  lm(team.stats.ball_in_box ~
     opp.stats.forced_turnovers +
     opp.stats.passes_pressured +
     opp.stats.pass_break_ups +
     opp.stats.pass_intercepts +
     opp.stats.long_passes,
     data = aleague_team_opp_data)

summary(attacking_pressure_rating)

aleague_team_opp_data$team.attacking_pressure_rating <- predict(attacking_pressure_rating, aleague_team_opp_data)
aleague_team_opp_data$team.attacking_pressure_rating <- aleague_team_opp_data$team.attacking_pressure_rating/mean(aleague_team_opp_data$team.attacking_pressure_rating, na.rm = TRUE)

#### Ball in Box Physicality Rating ####

attacking_physicality_rating <- 
  
  lm(team.stats.ball_in_box ~
     team.stats.aerial_contests_attempted +
     team.stats.aerial_contests_won_percentage +
     team.stats.contests_attempted +
     team.stats.contests_won_percentage +
     team.stats.aerial_contests_won_percentage +
     team.stats.fouls_conceded +
     team.stats.fouls_won +
     team.stats.tackles +
     team.stats.red_cards +
     team.stats.yellow_cards +
     team.stats.tackle_attempts +
     team.stats.tackles_won 
     ,
     data = aleague_team_opp_data)

summary(attacking_physicality_rating)

aleague_team_opp_data$team.attacking_physicality_rating <- predict(attacking_physicality_rating, aleague_team_opp_data)
aleague_team_opp_data$team.attacking_physicality_rating <- aleague_team_opp_data$team.attacking_physicality_rating/mean(aleague_team_opp_data$team.attacking_physicality_rating, na.rm = TRUE)

#### Ball in Box Possession Quality Rating ####

attacking_quality_rating <- 
  
  lm(team.stats.ball_in_box ~
     team.stats.crosses_completed +
     opp.stats.clearance_attempts +
     team.stats.through_balls_complete +
     team.stats.attacking_third_passes +
     team.stats.attacking_third_passes_percentage 
     ,
     data = aleague_team_opp_data)

summary(attacking_quality_rating)

aleague_team_opp_data$team.attacking_quality_rating <- predict(attacking_quality_rating, aleague_team_opp_data)
aleague_team_opp_data$team.attacking_quality_rating <- aleague_team_opp_data$team.attacking_quality_rating/mean(aleague_team_opp_data$team.attacking_quality_rating, na.rm = TRUE)



#### Per Possession Metrics
aleague_team_opp_data <- aleague_team_opp_data %>% 
  dplyr::mutate(team.possessions_rating = team.stats.possessions,
                team.crosses_rating =  team.stats.crosses/team.stats.possessions,
                team.shots_rating =  team.stats.shots/team.stats.possessions,
                team.shots_on_target_rating = team.stats.shots_on_target/team.stats.shots,
                team.efficiency_rating = team.stats.goals/team.stats.shots,
                team.total_crosses_rating = team.stats.crosses,
                team.total_ball_in_box_rating = team.stats.ball_in_box,
                
                
                
                team.possessions_rating = team.possessions_rating/mean(team.possessions_rating, na.rm = TRUE),
                team.crosses_rating =  team.crosses_rating/mean(team.crosses_rating, na.rm = TRUE),
                team.shots_rating =  team.shots_rating/mean(team.shots_rating, na.rm = TRUE),
                team.shots_on_target_rating = team.shots_on_target_rating/mean(team.shots_on_target_rating, na.rm = TRUE),
                team.efficiency_rating = team.efficiency_rating/mean(team.efficiency_rating, na.rm = TRUE),
                team.total_crosses_rating = team.total_crosses_rating/mean(team.total_crosses_rating, na.rm = TRUE),
                team.total_ball_in_box_rating = team.total_ball_in_box_rating/mean(team.total_ball_in_box_rating, na.rm = TRUE),
                
                
                
                #opp
                
                opp.possessions_rating = opp.stats.possessions,
                opp.crosses_rating =  opp.stats.crosses/opp.stats.possessions,
                opp.shots_rating =  opp.stats.shots/opp.stats.possessions,
                opp.shots_on_target_rating = opp.stats.shots_on_target/opp.stats.shots,
                opp.efficiency_rating = opp.stats.goals/opp.stats.shots,
                opp.total_crosses_rating = opp.stats.crosses,
                opp.total_ball_in_box_rating = opp.stats.ball_in_box,
                
                
                
                opp.possessions_rating = opp.possessions_rating/mean(opp.possessions_rating, na.rm = TRUE),
                opp.crosses_rating =  opp.crosses_rating/mean(opp.crosses_rating, na.rm = TRUE),
                opp.shots_rating =  opp.shots_rating/mean(opp.shots_rating, na.rm = TRUE),
                opp.shots_on_target_rating = opp.shots_on_target_rating/mean(opp.shots_on_target_rating, na.rm = TRUE),
                opp.efficiency_rating = opp.efficiency_rating/mean(opp.efficiency_rating, na.rm = TRUE),
                opp.total_crosses_rating = opp.total_crosses_rating/mean(opp.total_crosses_rating, na.rm = TRUE),
                opp.total_ball_in_box_rating = opp.total_ball_in_box_rating/mean(opp.total_ball_in_box_rating, na.rm = TRUE),
                
                
                
                
                
)


#### Calculate Current Team Ratings ####

#### Possession
team.attacking_possession_rating <- lm(team.attacking_possession_rating ~ team.home.advantage.cat + team + opp.team, data = aleague_team_opp_data, 
                                       weights=(date.weight))


summary(team.attacking_possession_rating)


#### Pressire
team.attacking_pressure_rating <- lm(team.attacking_pressure_rating ~ team.home.advantage.cat + team + opp.team, data = aleague_team_opp_data, 
                                     weights=(date.weight))


summary(team.attacking_pressure_rating)

#### Physicality
team.attacking_physicality_rating <- lm(team.attacking_physicality_rating ~ team.home.advantage.cat + team + opp.team, data = aleague_team_opp_data, 
                                        weights=(date.weight))


summary(team.attacking_physicality_rating)

#### Quality
team.attacking_quality_rating <- lm(team.attacking_quality_rating ~ team.home.advantage.cat + team + opp.team, data = aleague_team_opp_data, 
                                    weights=(date.weight))


summary(team.attacking_quality_rating)

#### Possessions
team.possessions_rating <- lm(team.possessions_rating ~ team.home.advantage.cat + team + opp.team, data = aleague_team_opp_data, 
                              weights=(date.weight))

summary(team.possessions_rating)



#### Crosses
team.crosses_rating <- lm(team.crosses_rating ~ team.home.advantage.cat + team + opp.team, data = aleague_team_opp_data, 
                          weights=(date.weight))


summary(team.crosses_rating)


#### Shots
team.shots_rating <- lm(team.shots_rating ~ team.home.advantage.cat + team + opp.team, data = aleague_team_opp_data, 
                        weights=(date.weight))


summary(team.shots_rating)


#### Shots on Target
team.shots_on_target_rating <- lm(team.shots_on_target_rating ~ team.home.advantage.cat + team + opp.team, data = aleague_team_opp_data, 
                                  weights=(date.weight))


summary(team.shots_on_target_rating)


#### Efficiency
team.efficiency_rating <- lm(team.efficiency_rating ~ team.home.advantage.cat + team + opp.team, data = aleague_team_opp_data, 
                                    weights=(date.weight))

summary(team.efficiency_rating)

#### Total Frosses
team.total_crosses_rating <- lm(team.total_crosses_rating ~ team.home.advantage.cat + team + opp.team, data = aleague_team_opp_data, 
                             weights=(date.weight))

summary(team.total_crosses_rating)

#### Total Ball in Box
team.total_ball_in_box_rating <- lm(team.total_ball_in_box_rating ~ team.home.advantage.cat + team + opp.team, data = aleague_team_opp_data, 
                                weights=(date.weight))

summary(team.total_ball_in_box_rating)


#### Predictions for Home Team

aleague_predictions_data$predicted.home.efficiency.rating <- predict(team.efficiency_rating, 
                                                         data.frame(team.home.advantage.cat = aleague_predictions_data$home.advantage.cat,
                                                                    team = aleague_predictions_data$home.team,
                                                                    opp.team = aleague_predictions_data$away.team
                                                         ), type="response")

aleague_predictions_data$predicted.home.shots_on_target_rating <- predict(team.shots_on_target_rating, 
                                                                     data.frame(team.home.advantage.cat = aleague_predictions_data$home.advantage.cat,
                                                                                team = aleague_predictions_data$home.team,
                                                                                opp.team = aleague_predictions_data$away.team
                                                                     ), type="response")

aleague_predictions_data$predicted.home.shots.rating <- predict(team.shots_rating, 
                                                                     data.frame(team.home.advantage.cat = aleague_predictions_data$home.advantage.cat,
                                                                                team = aleague_predictions_data$home.team,
                                                                                opp.team = aleague_predictions_data$away.team
                                                                     ), type="response")

aleague_predictions_data$predicted.home.possessions.rating <- predict(team.possessions_rating, 
                                                                data.frame(team.home.advantage.cat = aleague_predictions_data$home.advantage.cat,
                                                                           team = aleague_predictions_data$home.team,
                                                                           opp.team = aleague_predictions_data$away.team
                                                                ), type="response")

aleague_predictions_data$predicted.home.total_crosses.rating <- predict(team.total_crosses_rating, 
                                                                      data.frame(team.home.advantage.cat = aleague_predictions_data$home.advantage.cat,
                                                                                 team = aleague_predictions_data$home.team,
                                                                                 opp.team = aleague_predictions_data$away.team
                                                                      ), type="response")


aleague_predictions_data$predicted.home.total_ball_in_box.rating <- predict(team.total_ball_in_box_rating, 
                                                                        data.frame(team.home.advantage.cat = aleague_predictions_data$home.advantage.cat,
                                                                                   team = aleague_predictions_data$home.team,
                                                                                   opp.team = aleague_predictions_data$away.team
                                                                        ), type="response")


#predictions for away Team

aleague_predictions_data$predicted.away.efficiency.rating <- predict(team.efficiency_rating, 
                                                                     data.frame(team.home.advantage.cat = -aleague_predictions_data$home.advantage.cat,
                                                                                opp.team = aleague_predictions_data$home.team,
                                                                                team = aleague_predictions_data$away.team
                                                                     ), type="response")

aleague_predictions_data$predicted.away.shots_on_target_rating <- predict(team.shots_on_target_rating, 
                                                                          data.frame(team.home.advantage.cat = -aleague_predictions_data$home.advantage.cat,
                                                                                     opp.team = aleague_predictions_data$home.team,
                                                                                     team = aleague_predictions_data$away.team
                                                                          ), type="response")

aleague_predictions_data$predicted.away.shots.rating <- predict(team.shots_rating, 
                                                                data.frame(team.home.advantage.cat = -aleague_predictions_data$home.advantage.cat,
                                                                           opp.team = aleague_predictions_data$home.team,
                                                                           team = aleague_predictions_data$away.team
                                                                ), type="response")

aleague_predictions_data$predicted.away.possessions.rating <- predict(team.possessions_rating, 
                                                                      data.frame(team.home.advantage.cat = -aleague_predictions_data$home.advantage.cat,
                                                                                 opp.team = aleague_predictions_data$home.team,
                                                                                 team = aleague_predictions_data$away.team
                                                                      ), type="response")

aleague_predictions_data$predicted.away.total_crosses.rating <- predict(team.total_crosses_rating, 
                                                                        data.frame(team.home.advantage.cat = -aleague_predictions_data$home.advantage.cat,
                                                                                   opp.team = aleague_predictions_data$home.team,
                                                                                   team = aleague_predictions_data$away.team
                                                                        ), type="response")


aleague_predictions_data$predicted.away.total_ball_in_box.rating <- predict(team.total_ball_in_box_rating, 
                                                                            data.frame(team.home.advantage.cat = -aleague_predictions_data$home.advantage.cat,
                                                                                       opp.team = aleague_predictions_data$home.team,
                                                                                       team = aleague_predictions_data$away.team
                                                                            ), type="response")








# #### Merge to Home and Away DF
# #merge to home and away data frames
# aleague_team_opp_data_home <- aleague_team_opp_data %>% 
#   dplyr::filter(home == "Home")
# 
# names(aleague_team_opp_data_home) <- gsub(x = names(aleague_team_opp_data_home), pattern = "team.", replacement = "home.") 
# 
# aleague_team_opp_data_home <- aleague_team_opp_data_home %>% 
#   dplyr::ungroup() %>% 
#   dplyr::select(match_id, 
#                 home.efficiency_rating, 
#                 home.shots_on_target_rating,
#                 home.shots_rating, 
#                 home.possessions_rating, 
#                 home.crosses_rating, 
#                 home.total_crosses_rating,
#                 home.total_ball_in_box_rating,
#                 
#                 home.attacking_quality_rating, 
#                 home.attacking_possession_rating,
#                 home.attacking_physicality_rating,
#                 home.attacking_pressure_rating) 
# 
# aleague_team_opp_data_away <- aleague_team_opp_data %>% 
#   dplyr::filter(home == "Away")
# 
# names(aleague_team_opp_data_away) <- gsub(x = names(aleague_team_opp_data_away), pattern = "team.", replacement = "away.") 
# 
# aleague_team_opp_data_away <- aleague_team_opp_data_away %>% 
#   dplyr::ungroup() %>% 
#   dplyr::select(match_id, 
#                 away.efficiency_rating, 
#                 away.shots_on_target_rating,
#                 away.shots_rating, 
#                 away.possessions_rating, 
#                 away.crosses_rating, 
#                 away.total_crosses_rating,
#                 away.total_ball_in_box_rating,
#                 
#                 away.attacking_quality_rating, 
#                 away.attacking_possession_rating,
#                 away.attacking_physicality_rating,
#                 away.attacking_pressure_rating)
# 
# aleague_match_data_team_ratings <- left_join(aleague_team_opp_data_home, aleague_team_opp_data_away, by = c("match_id"))
# 
# 
# aleague_match_data_team_ratings
# 
# 
# aleague_match_data <- left_join(aleague_match_data, aleague_match_data_team_ratings, by = c("match_id"))
# 
# 
# #### Variable Importance
# 
# team_corners <- lm(home.stats.corners_taken ~ 
#                      home.advantage.cat +
#                      elo.diff + 
#                      corners_elo.diff +
#                      total.goals_band +
#                      rainfall.cat +
#                      max_temperature.cat + 
#                      
#                      home.efficiency_rating + 
#                      home.shots_on_target_rating +
#                      home.shots_rating + 
#                      home.possessions_rating + 
#                      home.crosses_rating + 
#                      home.total_crosses_rating +
#                      home.total_ball_in_box_rating +
#                      
#                      home.attacking_quality_rating + 
#                      home.attacking_possession_rating +
#                      home.attacking_physicality_rating +
#                      home.attacking_pressure_rating +
#                      
#                      away.efficiency_rating + 
#                      away.shots_on_target_rating +
#                      away.shots_rating + 
#                      away.possessions_rating + 
#                      away.crosses_rating + 
#                      away.total_crosses_rating +
#                      away.total_ball_in_box_rating +
#                      
#                      away.attacking_quality_rating + 
#                      away.attacking_possession_rating +
#                      away.attacking_physicality_rating +
#                      away.attacking_pressure_rating
#                    
#                    
#                    
#                    
#                    
#                    ,
#                    data = aleague_match_data,
#                    weights=(date.weight))
# 
# summary(team_corners)
# 
# 
# home_team_corners <- lm(home.stats.corners_taken ~ 
#                           home.advantage.cat +
#                           elo.diff + 
#                           corners_elo.diff +
#                           total.goals_band +
#                           rainfall.cat +
#                           max_temperature.cat + 
#                           
#                           home.efficiency_rating + 
#                           home.shots_on_target_rating +
#                           home.shots_rating + 
#                           home.possessions_rating + 
#                           home.total_crosses_rating +
#                           home.total_ball_in_box_rating +
#                           
#                           away.efficiency_rating + 
#                           away.shots_on_target_rating +
#                           away.shots_rating + 
#                           away.possessions_rating +
#                           away.total_crosses_rating +
#                           away.total_ball_in_box_rating
#                         ,
#                         data = aleague_match_data,
#                         weights=(date.weight))
# 
# summary(home_team_corners)
# 
# 
# 
# 
# away_team_corners <- lm(away.stats.corners_taken ~ 
#                           home.advantage.cat +
#                           elo.diff + 
#                           corners_elo.diff +
#                           total.goals_band +
#                           rainfall.cat +
#                           max_temperature.cat + 
#                           
#                           home.efficiency_rating + 
#                           home.shots_on_target_rating +
#                           home.shots_rating + 
#                           home.possessions_rating + 
#                           home.total_crosses_rating +
#                           home.total_ball_in_box_rating +
#                           
#                           away.efficiency_rating + 
#                           away.shots_on_target_rating +
#                           away.shots_rating + 
#                           away.possessions_rating +
#                           away.total_crosses_rating +
#                           away.total_ball_in_box_rating
#                         ,
#                         data = aleague_match_data,
#                         weights=(date.weight))
# 
# summary(away_team_corners)
