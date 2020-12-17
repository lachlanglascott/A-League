training_data <-  aleague_team_opp_data %>% filter(season > 2017)

fit.xgb <- train(team.stats.corners_taken ~ 
                        team.home.advantage.cat +
                        elo.diff + 
                        corners_elo.diff +
                        total.goals_band +
                        rainfall.cat +
                        max_temperature.cat + 
                        
                        team.efficiency_rating + 
                        team.shots_on_target_rating +
                        team.shots_rating + 
                        team.possessions_rating + 
                        team.total_crosses_rating +
                        team.total_ball_in_box_rating +
                        
                        team.attacking_quality_rating + 
                        team.attacking_possession_rating +
                        team.attacking_physicality_rating +
                        team.attacking_pressure_rating +
                        
                        #opp.attacking_quality_rating + 
                        #opp.attacking_possession_rating +
                        #opp.attacking_physicality_rating +
                        #opp.attacking_pressure_rating  +
                        
                        opp.efficiency_rating + 
                        opp.shots_on_target_rating +
                        opp.shots_rating + 
                        opp.possessions_rating +
                        opp.total_crosses_rating +
                        opp.total_ball_in_box_rating + 
                      
                        team.est_mins_trailing
                      
                      ,
                      data = training_data,
                      method="xgbTree", preProc=c("center", "scale"),
                      weights=date.weight)

print(fit.xgb)

#### Linear regresssion
team_corners_elo <- lm(team.stats.corners_taken ~ 
                         team.home.advantage.cat +
                         elo.diff + 
                         corners_elo.diff +
                         total.goals_band +
                         rainfall.cat +
                         max_temperature.cat +
                         rolling.team.stats.corners_taken +
                         rolling.team.stats.corners_taken_against +
                         rolling.opp.stats.corners_taken +
                         rolling.opp.stats.corners_taken_against 
                       
                       #team.est_mins_trailing
                       
                       ,
                       data = aleague_team_opp_data,
                       weights=(date.weight))

summary(team_corners_elo)
