
library(tidyverse)

aleague_player_ratings <- aleague_player_data %>% 
  dplyr::group_by(full_name) %>% 
  dplyr::summarise(stats.fantasy_points.average = mean(stats.fantasy_points))
                   

# nrl_player_ratings <- left_join(nrl_player_ratings, nrl_players, by = c("full_name"))
# 
# nrl_player_ratings <- nrl_player_ratings %>% 
#   dplyr::group_by(team) %>%
#   dplyr::arrange(desc(stats.fantasy_points.average)) %>% 
#   dplyr::filter(is.na(weeks) == TRUE) %>% 
#   top_n(11, stats.fantasy_points.average) %>% 
#   dplyr::summarise(stats.fantasy_points.team = sum(stats.fantasy_points.average)) %>% 
#   dplyr::arrange(desc(stats.fantasy_points.team)) 
# 
# nrl_player_ratings_avg <- mean(nrl_player_ratings$stats.fantasy_points.team)/mean(nrl_team$stats.fantasy_points)
# 
# nrl_player_ratings <- nrl_player_ratings %>% 
#   dplyr::filter(is.na(team) == FALSE) %>% 
#   dplyr::mutate(stats.fantasy_points.team = stats.fantasy_points.team/nrl_player_ratings_avg)
# 
# nrl_player_ratings <- rbind(nrl_player_ratings_past, nrl_player_ratings)
# 
# 
# write.csv(nrl_player_ratings, file = "C:/Users/glascottl/OneDrive - Tabcorp/Documents/R/Model data/nrl/nrl_player_data.csv")
# 
# nrl_fixture <- left_join(nrl_fixture, nrl_player_ratings, by = c('home.team' = 'team', "season" = "season", "round.number" = "round")) %>% 
#   dplyr::rename(home.player_ratings = stats.fantasy_points.team)
# 
# nrl_fixture <- left_join(nrl_fixture, nrl_player_ratings, by = c('away.team' = 'team', "season" = "season", "round.number" = "round")) %>% 
#   dplyr::rename(away.player_ratings = stats.fantasy_points.team)
# 
# 
# nrl_match_stats <- left_join(nrl_match_stats, nrl_player_ratings, by = c('team' = 'team', "season" = "season", "round.number" = "round")) %>% 
#   dplyr::rename(team.player_ratings = stats.fantasy_points.team)
# 
# nrl_match_stats <- left_join(nrl_match_stats, nrl_player_ratings, by = c('opp.team' = 'team', "season" = "season", "round.number" = "round")) %>% 
#   dplyr::rename(opp.team.player_ratings = stats.fantasy_points.team)
# 
# 
# #number of better than average players
# nrl_players_average <- nrl_data %>% 
#   dplyr::group_by(full_name) %>% 
#   dplyr::filter(season %in% c(season.var-1, season.var)) %>% 
#   dplyr::summarise(player.fantasy_points.average = mean(stats.fantasy_points)) 
# 
# nrl_players_preferred_position <- nrl_data %>% 
#   dplyr::group_by(full_name, position) %>% 
#   dplyr::filter(season %in% c(season.var-1, season.var)) %>% 
#   dplyr::summarise(n = n()) %>%
#   top_n(1) %>% 
#   filter(row_number()==1)
# 
# nrl_players_list <- left_join(nrl_players_average, nrl_players_preferred_position, by = "full_name")
# 
# nrl_position_average <- nrl_data %>% 
#   dplyr::group_by(position) %>% 
#   dplyr::filter(season %in% c(season.var-1, season.var)) %>% 
#   dplyr::summarise(position.fantasy_points.average = mean(stats.fantasy_points),
#                    position.fantasy_points.median = median(stats.fantasy_points)) 
# 
# nrl_team_consideration <- nrl_data %>% 
#   dplyr::group_by(team,full_name) %>% 
#   dplyr::filter(season %in% c(season.var-1, season.var)) %>% 
#   dplyr::summarise(fantasy_points.average = mean(stats.fantasy_points)) %>% 
#   arrange(desc(fantasy_points.average)) %>% 
#   ungroup() %>% 
#   group_by(team) %>% 
#   mutate(rank = row_number()) %>% 
#   ungroup() %>% 
#   select(-team) %>% 
#   group_by(full_name) %>% 
#   top_n(1,rank)
# 
# nrl_players_list <- left_join(nrl_players_list, nrl_position_average, by = "position")
# nrl_players_list <- left_join(nrl_players_list, nrl_team_consideration, by = "full_name")
# 
# 
# nrl_players_list <- nrl_players_list %>% 
#   dplyr::mutate(player_better_than_avg = ifelse(position != "Interchange" & n > 5 & player.fantasy_points.average > position.fantasy_points.average, 1, 0),
#                 player_better_than_median = ifelse(position != "Interchange" & n > 10 & player.fantasy_points.average > position.fantasy_points.median, 1, 0),
#                 player_elite = ifelse(position != "Interchange" & n > 10 & player.fantasy_points.average > position.fantasy_points.average+10, 1, 0),
#                 player_top5_team = ifelse(position != "Interchange"  & n > 10 & rank < 10,1,0),
#                 player_better_than_average = ifelse((player_better_than_avg + player_better_than_median + player_top5_team) > 0,1,0) 
#   )
# 
# nrl_players_list %>% 
#   filter(player_elite == 1)
# 
# nrl_players_list %>% 
#   filter(player_better_than_average == 1)
# 
# good_players <- nrl_players_list %>% 
#   filter(player_better_than_average == 1) %>% 
#   dplyr::select(full_name) %>% as.matrix()
# 
# good_player_counts <- nrl_data %>% 
#   filter(full_name %in% good_players, season > 2017) %>% 
#   group_by(match_id, season, team) %>%
#   summarise(good_players = n())
# 
# ##group by season, team and get the max good players
# good_players_max <- nrl_data %>% 
#   filter(full_name %in% good_players, season > 2017) %>% 
#   group_by(match_id, season,  team) %>%
#   summarise(good_players_max = n()) %>% 
#   ungroup() %>% 
#   dplyr::select(season, team, good_players_max) %>% 
#   group_by(season, team) %>% 
#   top_n(1, good_players_max) %>% 
#   filter(row_number()==1)
# 
# #by position
# good_player_counts_position <- nrl_data %>% 
#   filter(full_name %in% good_players, season > 2017) %>% 
#   group_by(match_id, season, team, position) %>%
#   summarise(good_players = n()) %>% 
#   filter((position != "Did Not Play")) %>%
#   filter((position != "Interchange")) %>% 
#   reshape2::dcast(match_id + season + team ~ position, value.var = "good_players") %>% 
#   mutate_all(~replace(., is.na(.), 0))
# 
# names(good_player_counts_position) <- paste(names(good_player_counts_position),".count", sep = "", collapse = NULL)
# names(good_player_counts_position) <- tolower(names(good_player_counts_position))
# names(good_player_counts_position) <- gsub(x = names(good_player_counts_position), pattern = " ", replacement = ".")
# 
# good_players_max_position <- nrl_data %>% 
#   filter(full_name %in% good_players, season > 2017) %>% 
#   group_by(match_id, season,  team, position) %>%
#   summarise(good_players_max = n()) %>% 
#   ungroup() %>% 
#   dplyr::select(season, team, position,good_players_max) %>% 
#   group_by(season, team, position) %>% 
#   top_n(1, good_players_max) %>% 
#   filter(row_number()==1) %>% 
#   filter((position != "Did Not Play")) %>%
#   filter((position != "Interchange")) %>% 
#   reshape2::dcast(season + team ~ position, value.var = "good_players_max") %>% 
#   mutate_all(~replace(., is.na(.), 0))
# 
# names(good_players_max_position) <- paste(names(good_players_max_position),".max", sep = "", collapse = NULL)
# names(good_players_max_position) <- tolower(names(good_players_max_position))
# names(good_players_max_position) <- gsub(x = names(good_players_max_position), pattern = " ", replacement = ".")
# 
# #join with counts
# good_player_counts <- left_join(good_player_counts, good_players_max, by = c("team", "season"))
# 
# good_player_counts$good_players_missing <- good_player_counts$good_players_max - good_player_counts$good_players
# 
# good_player_counts_team <- good_player_counts %>% rename(team.good_players = good_players,
#                                                          team.good_players_missing = good_players_missing) %>% 
#   ungroup() %>% 
#   mutate(match_id = as.character(match_id))
# 
# good_player_counts_opp <- good_player_counts %>% rename(opp.team.good_players = good_players,
#                                                         opp.team.good_players_missing = good_players_missing,
#                                                         opp.team = team) %>% 
#   ungroup() %>% 
#   mutate(match_id = as.character(match_id))
# 
# nrl_match_stats <- left_join(nrl_match_stats, good_player_counts_team, by = c("team", "match_id", "season"))
# nrl_match_stats <- left_join(nrl_match_stats, good_player_counts_opp, by = c("opp.team", "match_id", "season"))
# 
# nrl_match_stats$good.players.diff <- nrl_match_stats$team.good_players - nrl_match_stats$opp.team.good_players
# nrl_match_stats$good.players.missing.diff <- nrl_match_stats$team.good_players_missing - nrl_match_stats$opp.team.good_players_missing
# 
# 
# #get it by position
# good_player_counts_position <- left_join(good_player_counts_position, good_players_max_position, by = c("team.count" = "team.max", 
#                                                                                                         "season.count" = "season.max"))
# 
# good_player_counts_position <- good_player_counts_position %>% 
#   dplyr::mutate(centre = centre.max - centre.count,
#                 five.eighth = five.eighth.max - five.eighth.count,
#                 full.back = full.back.max - full.back.count,
#                 halfback = halfback.max - halfback.count,
#                 hooker = hooker.max - hooker.count,
#                 lock = lock.max - lock.count,
#                 prop.forward = prop.forward.max - prop.forward.count,
#                 second.row = second.row.max - second.row.count,
#                 wing = wing.max - wing.count
#   )
# 
# good_player_counts_team_position <- good_player_counts_position %>% 
#   rename(match_id = match_id.count,
#          season = season.count,
#          team = team.count,
#          team.good.players.centre = centre.count,
#          team.good.players.five.eighth = five.eighth.count,
#          team.good.players.full.back = full.back.count,
#          team.good.players.halfback = halfback.count,
#          team.good.players.hooker = hooker.count,
#          team.good.players.lock = lock.count,
#          team.good.players.prop.forward = prop.forward.count,
#          team.good.players.second.row = second.row.count,
#          team.good.players.wing = wing.count,
#          team.good.players.centre.squad = centre.max,
#          team.good.players.five.eighth.squad = five.eighth.max,
#          team.good.players.full.back.squad = full.back.max,
#          team.good.players.halfback.squad = halfback.max,
#          team.good.players.hooker.squad = hooker.max,
#          team.good.players.lock.squad = lock.max,
#          team.good.players.prop.forward.squad = prop.forward.max,
#          team.good.players.second.row.squad = second.row.max,
#          team.good.players.wing.squad = wing.max,
#          team.good.players.centre.missing = centre,
#          team.good.players.five.eighth.missing = five.eighth,
#          team.good.players.full.back.missing = full.back,
#          team.good.players.halfback.missing = halfback,
#          team.good.players.hooker.missing = hooker,
#          team.good.players.lock.missing = lock,
#          team.good.players.prop.forward.missing= prop.forward,
#          team.good.players.second.row.missing = second.row,
#          team.good.players.wing.missing = wing
#   ) %>% 
#   ungroup() %>% 
#   mutate(match_id = as.character(match_id))
# 
# good_player_counts_opp_position <- good_player_counts_position %>% 
#   rename(match_id = match_id.count,
#          season = season.count,
#          opp.team = team.count,
#          opp.team.good.players.centre = centre.count,
#          opp.team.good.players.five.eighth = five.eighth.count,
#          opp.team.good.players.full.back = full.back.count,
#          opp.team.good.players.halfback = halfback.count,
#          opp.team.good.players.hooker = hooker.count,
#          opp.team.good.players.lock = lock.count,
#          opp.team.good.players.prop.forward = prop.forward.count,
#          opp.team.good.players.second.row = second.row.count,
#          opp.team.good.players.wing = wing.count,
#          opp.team.good.players.centre.squad = centre.max,
#          opp.team.good.players.five.eighth.squad = five.eighth.max,
#          opp.team.good.players.full.back.squad = full.back.max,
#          opp.team.good.players.halfback.eighth.squad = halfback.max,
#          opp.team.good.players.hooker.squad = hooker.max,
#          opp.team.good.players.lock.squad = lock.max,
#          opp.team.good.players.prop.forward.squad = prop.forward.max,
#          opp.team.good.players.second.row.squad = second.row.max,
#          opp.team.good.players.wing.squad = wing.max,
#          opp.team.good.players.centre.missing = centre,
#          opp.team.good.players.five.eighth.missing = five.eighth,
#          opp.team.good.players.full.back.missing = full.back,
#          opp.team.good.players.halfback.missing = halfback,
#          opp.team.good.players.hooker.missing = hooker,
#          opp.team.good.players.lock.missing = lock,
#          opp.team.good.players.prop.forward.missing = prop.forward,
#          opp.team.good.players.second.row.missing = second.row,
#          opp.team.good.players.wing.missing = wing
#   ) %>% 
#   ungroup() %>% 
#   mutate(match_id = as.character(match_id))
# 
# nrl_match_stats <- left_join(nrl_match_stats, good_player_counts_team_position, by = c("team", "match_id", "season"))
# nrl_match_stats <- left_join(nrl_match_stats, good_player_counts_opp_position, by = c("opp.team", "match_id", "season"))
# 
# nrl_match_stats <- nrl_match_stats  %>% 
#   dplyr::mutate(good.players.centre.diff = team.good.players.centre - opp.team.good.players.centre,
#                 good.players.centre.missing.diff = team.good.players.centre.missing - opp.team.good.players.centre.missing,
#                 
#                 good.players.five.eighth.diff = team.good.players.five.eighth - opp.team.good.players.five.eighth,
#                 good.players.five.eighth.missing.diff = team.good.players.five.eighth.missing - opp.team.good.players.five.eighth.missing,
#                 
#                 good.players.full.back.diff = team.good.players.full.back - opp.team.good.players.full.back,
#                 good.players.full.back.missing.diff = team.good.players.full.back.missing - opp.team.good.players.full.back.missing,
#                 
#                 good.players.halfback.diff = team.good.players.halfback - opp.team.good.players.halfback,
#                 good.players.halfback.missing.diff = team.good.players.halfback.missing - opp.team.good.players.halfback.missing,
#                 
#                 good.players.hooker.diff = team.good.players.hooker - opp.team.good.players.hooker,
#                 good.players.hooker.missing.diff = team.good.players.hooker.missing - opp.team.good.players.hooker.missing,
#                 
#                 good.players.lock.diff = team.good.players.lock - opp.team.good.players.lock,
#                 good.players.lock.missing.diff = team.good.players.lock.missing - opp.team.good.players.lock.missing,
#                 
#                 good.players.prop.forward.diff = team.good.players.prop.forward - opp.team.good.players.prop.forward,
#                 good.players.prop.forward.missing.diff = team.good.players.prop.forward.missing - opp.team.good.players.prop.forward.missing,
#                 
#                 good.players.second.row.diff = team.good.players.second.row - opp.team.good.players.second.row,
#                 good.players.second.row.missing.diff = team.good.players.second.row.missing - opp.team.good.players.second.row.missing,
#                 
#                 good.players.wing.diff = team.good.players.wing - opp.team.good.players.wing,
#                 good.players.wing.missing.diff = team.good.players.wing.missing - opp.team.good.players.wing.missing
#   )
# 
# 
# 
# #nowe get good players on list in season and add to fixture
# 
# good_players_current <- nrl_data %>% 
#   filter(full_name %in% good_players, season == 2020) %>% 
#   dplyr::select(full_name, team) %>%
#   distinct()%>% 
#   group_by(team) %>% 
#   summarise(good_players_list = n())
# 
# nrl_injuries_good_players <- nrl_injuries %>%
#   mutate(good_player = ifelse((full_name %in% good_players), "Yes", "No"))
# 
# save(nrl_injuries_good_players, file = "nrl_injuries_good_players.RData")
# 
# good_players_injured <- nrl_injuries %>% filter(full_name %in% good_players) %>% dplyr::select(full_name, team) %>% distinct()%>% 
#   group_by(team) %>% 
#   summarise(good_players_injured = n())
# 
# good_players_df <- left_join(good_players_current, good_players_injured, by = "team")
# 
# good_players_df$good_players_injured <- ifelse(is.na(good_players_df$good_players_injured) == TRUE, 0, good_players_df$good_players_injured)
# good_players_df$good_players_available <- good_players_df$good_players_list - good_players_df$good_players_injured
# good_players_df$season <- season.var
# good_players_df$round.number <- round.var
# 
# good_players_df_home <- good_players_df %>% rename(home.team = team, 
#                                                    home.good_players_injured = good_players_injured,
#                                                    home.good_players_available = good_players_available,
#                                                    home.good_players_list = good_players_list)
# good_players_df_away <- good_players_df %>% rename(away.team = team,
#                                                    away.good_players_injured = good_players_injured,
#                                                    away.good_players_available = good_players_available,
#                                                    away.good_players_list = good_players_list)
# 
# nrl_fixture <- left_join(nrl_fixture, good_players_df_home, by = c("home.team", "season", "round.number"))
# nrl_fixture <- left_join(nrl_fixture, good_players_df_away, by = c("away.team", "season", "round.number"))
# 
# nrl_fixture$good.players.diff <- nrl_fixture$home.good_players_available - nrl_fixture$away.good_players_available
# nrl_fixture$good.players.missing.diff <- nrl_fixture$home.good_players_injured - nrl_fixture$away.good_players_injured
# 
# good_player_counts_home <- good_player_counts_team %>% 
#   rename(home.team = team,
#          home.team.good_players = team.good_players,
#          home.team.good_players_missing = team.good_players_missing) 
# 
# good_player_counts_away <- good_player_counts_team %>% 
#   rename(away.team = team,
#          away.team.good_players = team.good_players,
#          away.team.good_players_missing = team.good_players_missing)
# 
# 
# nrl_match_data <- left_join(nrl_match_data, good_player_counts_home, by = c("home.team", "match_id", "season"))
# nrl_match_data <- left_join(nrl_match_data, good_player_counts_away, by = c("away.team", "match_id", "season"))
# 
# nrl_match_data$good.players.diff <- nrl_match_data$home.team.good_players - nrl_match_data$away.team.good_players
# nrl_match_data$good.players.missing.diff <- nrl_match_data$home.team.good_players_missing - nrl_match_data$away.team.good_players_missing
