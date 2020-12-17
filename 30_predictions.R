#### Rolling Average ####

aleague_predictions_data$predicted_rollavg.home.corners.stats <- predict(fit.rollavg, 
                                                                         data.frame(
                                                                           
                                                                           rolling.team.stats.corners_taken = aleague_predictions_data$rolling.home.stats.corners_taken,
                                                                           rolling.opp.stats.corners_taken = aleague_predictions_data$rolling.home.stats.corners_taken,
                                                                           rolling.team.stats.corners_taken_against = aleague_predictions_data$rolling.away.stats.corners_taken_against,
                                                                           rolling.opp.stats.corners_taken_against = aleague_predictions_data$rolling.away.stats.corners_taken_against  
                                                                           
                                                                         ))

aleague_predictions_data$predicted_rollavg.away.corners.stats <- predict(fit.rollavg, 
                                                                         data.frame(
                                                                           
                                                                           rolling.team.stats.corners_taken = aleague_predictions_data$rolling.home.stats.corners_taken,
                                                                           rolling.opp.stats.corners_taken = aleague_predictions_data$rolling.away.stats.corners_taken,
                                                                           rolling.team.stats.corners_taken_against = aleague_predictions_data$rolling.home.stats.corners_taken_against,
                                                                           rolling.opp.stats.corners_taken_against = aleague_predictions_data$rolling.home.stats.corners_taken_against  
                                                                           
                                                                         ))


#### XG Boost ####

aleague_predictions_data$predicted_xgb.home.corners.stats <- predict(fit.xgb, 
                                                                     data.frame(team.home.advantage.cat = aleague_predictions_data$home.advantage.cat,
                                                                                elo.diff  =  aleague_predictions_data$elo.diff, 
                                                                                corners_elo.diff  = aleague_predictions_data$corners_elo.diff, 
                                                                                total.goals_band = aleague_predictions_data$total.corners_band.poisson,
                                                                                rainfall.cat = aleague_predictions_data$rainfall.cat, 
                                                                                max_temperature.cat = aleague_predictions_data$max_temperature.cat,
                                                                                
                                                                                team.efficiency_rating  = aleague_predictions_data$predicted.home.efficiency.rating, 
                                                                                team.shots_on_target_rating = aleague_predictions_data$predicted.away.shots_on_target_rating, 
                                                                                team.shots_rating = aleague_predictions_data$predicted.home.shots.rating, 
                                                                                team.possessions_rating = aleague_predictions_data$predicted.home.possessions.rating,
                                                                                team.total_crosses_rating = aleague_predictions_data$predicted.home.total_crosses.rating,
                                                                                team.total_ball_in_box_rating = aleague_predictions_data$predicted.home.total_ball_in_box.rating,
                                                                                
                                                                                team.attacking_quality_rating = aleague_predictions_data$predicted.home.attacking_quality_rating, 
                                                                                team.attacking_possession_rating = aleague_predictions_data$predicted.home.attacking_possession_rating, 
                                                                                team.attacking_physicality_rating = aleague_predictions_data$predicted.home.attacking_physicality_rating, 
                                                                                team.attacking_pressure_rating = aleague_predictions_data$predicted.home.attacking_pressure_rating, 
                                                                                
                                                                                
                                                                                opp.efficiency_rating  = aleague_predictions_data$predicted.away.efficiency.rating, 
                                                                                opp.shots_on_target_rating = aleague_predictions_data$predicted.away.shots_on_target_rating, 
                                                                                opp.shots_rating = aleague_predictions_data$predicted.away.shots.rating, 
                                                                                opp.possessions_rating = aleague_predictions_data$predicted.away.possessions.rating,
                                                                                opp.total_crosses_rating = aleague_predictions_data$predicted.away.total_crosses.rating,
                                                                                opp.total_ball_in_box_rating = aleague_predictions_data$predicted.away.total_ball_in_box.rating,
                                                                                
                                                                                team.est_mins_trailing = aleague_predictions_data$home.est_mins_trailing
                                                                                
                                                                     ))


aleague_predictions_data$predicted_xgb.away.corners.stats <- predict(fit.xgb, 
                                                                     data.frame(team.home.advantage.cat = -aleague_predictions_data$home.advantage.cat,
                                                                                elo.diff  =  -aleague_predictions_data$elo.diff, 
                                                                                corners_elo.diff  = -aleague_predictions_data$corners_elo.diff, 
                                                                                total.goals_band = aleague_predictions_data$total.corners_band.poisson,
                                                                                rainfall.cat = aleague_predictions_data$rainfall.cat, 
                                                                                max_temperature.cat = aleague_predictions_data$max_temperature.cat,
                                                                                
                                                                                opp.efficiency_rating  = aleague_predictions_data$predicted.home.efficiency.rating, 
                                                                                opp.shots_on_target_rating = aleague_predictions_data$predicted.away.shots_on_target_rating, 
                                                                                opp.shots_rating = aleague_predictions_data$predicted.home.shots.rating, 
                                                                                opp.possessions_rating = aleague_predictions_data$predicted.home.possessions.rating,
                                                                                opp.total_crosses_rating = aleague_predictions_data$predicted.home.total_crosses.rating,
                                                                                opp.total_ball_in_box_rating = aleague_predictions_data$predicted.home.total_ball_in_box.rating,
                                                                                
                                                                                team.attacking_quality_rating = aleague_predictions_data$predicted.away.attacking_quality_rating, 
                                                                                team.attacking_possession_rating = aleague_predictions_data$predicted.away.attacking_possession_rating, 
                                                                                team.attacking_physicality_rating = aleague_predictions_data$predicted.away.attacking_physicality_rating, 
                                                                                team.attacking_pressure_rating = aleague_predictions_data$predicted.away.attacking_pressure_rating, 
                                                                                
                                                                                team.efficiency_rating  = aleague_predictions_data$predicted.away.efficiency.rating, 
                                                                                team.shots_on_target_rating = aleague_predictions_data$predicted.away.shots_on_target_rating, 
                                                                                team.shots_rating = aleague_predictions_data$predicted.away.shots.rating, 
                                                                                team.possessions_rating = aleague_predictions_data$predicted.away.possessions.rating,
                                                                                team.total_crosses_rating = aleague_predictions_data$predicted.away.total_crosses.rating,
                                                                                team.total_ball_in_box_rating = aleague_predictions_data$predicted.away.total_ball_in_box.rating,
                                                                                
                                                                                team.est_mins_trailing = aleague_predictions_data$away.est_mins_trailing
                                                                                
                                                                     ))



aleague_predictions_data$predicted_elo.home.corners <- predict(team_corners_elo, 
                                                               data.frame(team.home.advantage.cat = aleague_predictions_data$home.advantage.cat,
                                                                          elo.diff  =  aleague_predictions_data$elo.diff, 
                                                                          corners_elo.diff  = aleague_predictions_data$corners_elo.diff, 
                                                                          total.goals_band = aleague_predictions_data$total.corners_band.poisson,
                                                                          
                                                                          rainfall.cat = aleague_predictions_data$rainfall.cat, 
                                                                          max_temperature.cat = aleague_predictions_data$max_temperature.cat,
                                                                          rolling.team.stats.corners_taken = aleague_predictions_data$rolling.team.stats.corners_taken,
                                                                          rolling.team.stats.corners_taken_against = aleague_predictions_data$rolling.team.stats.corners_taken_against,
                                                                          rolling.opp.stats.corners_taken  = aleague_predictions_data$rolling.opp.stats.corners_taken,
                                                                          rolling.opp.stats.corners_taken_against = aleague_predictions_data$rolling.opp.stats.corners_taken_against
                                                                          
                                                                          
                                                                          
                                                               ), type="response")


aleague_predictions_data$predicted_elo.away.corners <- predict(team_corners_elo, 
                                                               data.frame(team.home.advantage.cat = -aleague_predictions_data$home.advantage.cat,
                                                                          elo.diff  =  -aleague_predictions_data$elo.diff, 
                                                                          corners_elo.diff  = -aleague_predictions_data$corners_elo.diff, 
                                                                          total.goals_band = aleague_predictions_data$total.corners_band.poisson,
                                                                          
                                                                          rainfall.cat = aleague_predictions_data$rainfall.cat, 
                                                                          max_temperature.cat = aleague_predictions_data$max_temperature.cat,
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                               ), type="response")
