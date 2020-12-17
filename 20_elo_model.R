#### Team Strength Ratings ####

# Set parameters

map_margin_to_outcome <- function(margin, marg.max = 3, marg.min = -3){
  norm <- (margin - marg.min)/(marg.max - marg.min)
  norm %>% pmin(1) %>% pmax(0)
}

k_val <- 24
#inj_factor <- 16

df <- data.frame(names = character(0),facts = character(0),nm = character(0))

eloRating <- function(home="home.team", away="away.team", home.score="home.stats.goals",
                      away.score="away.stats.goals", data, kf=k_val, initialRating=1500,
                      homeAdvantage = 12,
                      homeAdvantageCat= "home.advantage.cat", 
                      #acl = "acl_affected",
                      #inj_factor = 16,
                      rev_mean=0.4){
  
  #Make a list to hold ratings for all teams
  all.teams <- levels(as.factor(union(levels(as.factor(data[[home]])),
                                      levels(as.factor(data[[away]])))))
  
  ratings <- as.list(rep(initialRating, times=length(all.teams)))
  names(ratings) <- all.teams
  
  tibble_list <- list()
  
  #Loop trough data and update ratings
  for (idx in 1:dim(data)[1]){
    
    #get current ratings
    home.team <- data[[home]][idx]
    away.team <- data[[away]][idx]
    home.team.rating <- as.numeric(ratings[home.team]) + (homeAdvantage*data[[homeAdvantageCat]][idx])
    away.team.rating <- as.numeric(ratings[away.team]) 
    
    # Reversion to the mean
    ratings[home.team] <- if_else(data[["round"]][idx] == "Round 1", (rev_mean * 1500) + (1 - rev_mean) * as.numeric(ratings[home.team]), as.numeric(ratings[home.team]))
    ratings[away.team] <- if_else(data[["round"]][idx] == "Round 1", (rev_mean * 1500) + (1 - rev_mean) * as.numeric(ratings[away.team]), as.numeric(ratings[away.team]))
    
    
    #calculate expected outcome 
    expected.home <- 1 / (1 + 10^((away.team.rating - home.team.rating)/400))
    expected.away <- 1 - expected.home
    
    #Observed outcome
    score.diff <- data[[home.score]][idx] - data[[away.score]][idx]
    
    result.home <- map_margin_to_outcome(data[[home.score]][idx] - data[[away.score]][idx])
    result.away <- 1-result.home
    
    # Tibble to store results
    tibble_list[[idx]] <- tibble(match_id = data[["match_id"]][idx], 
                                 season = data[["season"]][idx], 
                                 round = data[["round"]][idx],
                                 home.team = home.team, 
                                 away.team = away.team, 
                                 home.rating = unlist(ratings[home.team]), 
                                 away.rating = unlist(ratings[away.team]), 
                                 home.prob = expected.home, 
                                 away.prob = expected.away, 
                                 home.goals = data[["home.stats.goals"]][idx], 
                                 away.goals = data[["away.stats.goals"]][idx],
                                 home.result = result.home, 
                                 away.result = result.away
    )
    
    
    kfactor <- kf
    
    #update ratings
    ratings[home.team] <- as.numeric(ratings[home.team]) +  kfactor*(result.home - expected.home)
    ratings[away.team] <- as.numeric(ratings[away.team]) +  kfactor*(result.away - expected.away)
    
    tibble_list[[idx]] %>%
      mutate(updated.home.rating = unlist(ratings[home.team]), 
             updated.away.rating = unlist(ratings[away.team])) -> tibble_list[[idx]]
  }
  
  tibble_ratings <- bind_rows(tibble_list)
  
  #prepare output
  ratingsOut <- as.numeric(ratings)
  names(ratingsOut) <- names(ratings)
  ratingsOut <- sort(ratingsOut, decreasing=TRUE)
  
  return(tibble_ratings)
  
}

#### Load Ratings ####
elo <- eloRating(data=aleague_match_data)

current_elo_ratings <-
  rbind(
    data.frame(
      match_id = elo$match_id,
      team = elo$home.team, 
      team.elo = elo$updated.home.rating),
    data.frame(
      match_id = elo$match_id,
      team = elo$away.team, 
      team.elo = elo$updated.away.rating)) %>% 
  arrange(match_id) %>% 
  ungroup() %>% 
  group_by(team) %>% 
  summarise(team.elo = last(team.elo))

current_elo_ratings %>%  arrange(desc(team.elo))

current_elo_ratings_lag <-
  rbind(
    data.frame(
      match_id = elo$match_id,
      team = elo$home.team, 
      team.elo = elo$updated.home.rating),
    data.frame(
      match_id = elo$match_id,
      team = elo$away.team, 
      team.elo = elo$updated.away.rating)) %>% 
  
  arrange(desc(match_id)) %>% 
  ungroup() %>% 
  select(team, team.elo) %>% 
  group_by(team) %>% 
  filter(row_number()==3)

#### Join Elos to Match Level Data

elo_output <- cbind(elo[1:9]) %>% 
  rename(home.elo = home.rating,
         away.elo = away.rating,
         home.elo.prob = home.prob,
         away.elo.prob = away.prob)

aleague_match_data <- left_join(aleague_match_data, elo_output, by = c("season", "round", "home.team",
                                                               "away.team", "match_id"))

aleague_match_data$elo.diff <- aleague_match_data$home.elo - aleague_match_data$away.elo


aleague_match_data <- aleague_match_data %>% 
  dplyr::mutate(elo.diff.cat = ifelse(elo.diff > k_val, "Much Stronger Home",
                                      ifelse(elo.diff < -k_val, "Much Stronger Away", 
                                             ifelse(elo.diff > 0, "Stronger Home", "Stronger Away"))))

#### Join Elos to Team Level Data
elo_ratings_teams <-
  rbind(
    data.frame(
      match_id = elo$match_id,
      team = elo$home.team, 
      elo = elo$home.rating,
      elo.prob = elo$home.prob), 
    data.frame(
      match_id = elo$match_id,
      team = elo$away.team, 
      elo = elo$away.rating,
      elo.prob = elo$away.prob)) %>% 
  arrange(match_id) %>% 
  ungroup() %>% 
  group_by(team)

aleague_team_opp_data <- left_join(aleague_team_opp_data, elo_ratings_teams, by = c('match_id' = 'match_id', 'team' = 'team')) %>% 
  dplyr::rename(team.elo = elo,
                team.elo.prob = elo.prob) 

aleague_team_opp_data <- left_join(aleague_team_opp_data, elo_ratings_teams, by = c('match_id' = 'match_id', 'opp.team' = 'team')) %>% 
  dplyr::rename(opp.team.elo = elo,
                opp.team.elo.prob = elo.prob) %>% 
  dplyr::mutate(elo.diff = team.elo- opp.team.elo)


aleague_team_opp_data <- aleague_team_opp_data %>% 
  dplyr::mutate(elo.diff.cat = ifelse(elo.diff > k_val, "Much Stronger Team",
                                      ifelse(elo.diff < -k_val, "Much Stronger Opp", 
                                             ifelse(elo.diff > 0, "Stronger Team", "Stronger Opp"))))



#### Join to Predictions

current_elo_ratings_home <- current_elo_ratings %>% rename(home.team.elo = team.elo)
current_elo_ratings_away <- current_elo_ratings %>% rename(away.team.elo = team.elo)

aleague_predictions_data <- left_join(aleague_predictions_data, current_elo_ratings_home, by = c('home.team' = 'team'))
aleague_predictions_data <- left_join(aleague_predictions_data, current_elo_ratings_away, by = c('away.team' = 'team'))

aleague_predictions_data$no.crowd.factor = ifelse(aleague_predictions_data$season == 2020 & aleague_predictions_data$round.number > 1,2,1)

aleague_predictions_data$elo.diff <- aleague_predictions_data$home.team.elo - aleague_predictions_data$away.team.elo

aleague_predictions_data$home.elo.prob <- 1 / (1 + 10^((-(aleague_predictions_data$elo.diff))/400))
aleague_predictions_data$away.elo.prob <- 1 - aleague_predictions_data$home.elo.prob

aleague_predictions_data <- aleague_predictions_data %>% 
  dplyr::mutate(elo.diff.cat = ifelse(elo.diff > k_val, "Much Stronger Team",
                                      ifelse(elo.diff < -k_val, "Much Stronger Opp", 
                                             ifelse(elo.diff > 0, "Stronger Team", "Stronger Opp"))))




#### Team Corner Ratings ####

# Set parameters

map_margin_to_outcome <- function(margin, marg.max = 10, marg.min = -10){
  norm <- (margin - marg.min)/(marg.max - marg.min)
  norm %>% pmin(1) %>% pmax(0)
}

k_val <- 24
inj_factor <- 16

df <- data.frame(names = character(0),facts = character(0),nm = character(0))

corners_eloRating <- function(home="home.team", away="away.team", home.score="home.stats.corners_taken",
                      away.score="away.stats.corners_taken", data, kf=k_val, initialRating=1500,
                      homeAdvantage = 12,
                      homeAdvantageCat= "home.advantage.cat", 
                      #acl = "acl_affected",
                      #inj_factor = 16,
                      rev_mean=0.4){
  
  #Make a list to hold ratings for all teams
  all.teams <- levels(as.factor(union(levels(as.factor(data[[home]])),
                                      levels(as.factor(data[[away]])))))
  
  ratings <- as.list(rep(initialRating, times=length(all.teams)))
  names(ratings) <- all.teams
  
  tibble_list <- list()
  
  #Loop trough data and update ratings
  for (idx in 1:dim(data)[1]){
    
    #get current ratings
    home.team <- data[[home]][idx]
    away.team <- data[[away]][idx]
    home.team.rating <- as.numeric(ratings[home.team]) + ((homeAdvantage*data[[homeAdvantageCat]][idx]))
    away.team.rating <- as.numeric(ratings[away.team]) 
    
    # Reversion to the mean
    ratings[home.team] <- if_else(data[["round"]][idx] == "Round 1", (rev_mean * 1500) + (1 - rev_mean) * as.numeric(ratings[home.team]), as.numeric(ratings[home.team]))
    ratings[away.team] <- if_else(data[["round"]][idx] == "Round 1", (rev_mean * 1500) + (1 - rev_mean) * as.numeric(ratings[away.team]), as.numeric(ratings[away.team]))
    
    
    #calculate expected outcome 
    expected.home <- 1 / (1 + 10^((away.team.rating - home.team.rating)/400))
    expected.away <- 1 - expected.home
    
    #Observed outcome
    score.diff <- data[[home.score]][idx] - data[[away.score]][idx]
    
    result.home <- map_margin_to_outcome(data[[home.score]][idx] - data[[away.score]][idx])
    result.away <- 1-result.home
    
    # Tibble to store results
    tibble_list[[idx]] <- tibble(match_id = data[["match_id"]][idx], 
                                 season = data[["season"]][idx], 
                                 round = data[["round"]][idx],
                                 home.team = home.team, 
                                 away.team = away.team, 
                                 home.rating = unlist(ratings[home.team]), 
                                 away.rating = unlist(ratings[away.team]), 
                                 home.prob = expected.home, 
                                 away.prob = expected.away, 
                                 home.goals = data[["home.stats.goals"]][idx], 
                                 away.goals = data[["away.stats.goals"]][idx],
                                 home.result = result.home, 
                                 away.result = result.away
    )
    
    
    kfactor <- kf
    #kfactor <- 200/(data[["round.number"]][idx] + 20)^0.6
    
    #update ratings
    ratings[home.team] <- as.numeric(ratings[home.team]) + kfactor*(result.home - expected.home)
    ratings[away.team] <- as.numeric(ratings[away.team]) + kfactor*(result.away - expected.away)
    
    tibble_list[[idx]] %>%
      mutate(updated.home.rating = unlist(ratings[home.team]), 
             updated.away.rating = unlist(ratings[away.team])) -> tibble_list[[idx]]
  }
  
  tibble_ratings <- bind_rows(tibble_list)
  
  #prepare output
  ratingsOut <- as.numeric(ratings)
  names(ratingsOut) <- names(ratings)
  ratingsOut <- sort(ratingsOut, decreasing=TRUE)
  
  return(tibble_ratings)
  
}

#### Load Ratings ####
corners_elo <- corners_eloRating(data=aleague_match_data)

current_corners_elo_ratings <-
  rbind(
    data.frame(
      match_id = corners_elo$match_id,
      team = corners_elo$home.team, 
      team.corners_elo = corners_elo$updated.home.rating),
    data.frame(
      match_id = corners_elo$match_id,
      team = corners_elo$away.team, 
      team.corners_elo = corners_elo$updated.away.rating)) %>% 
  arrange(match_id) %>% 
  ungroup() %>% 
  group_by(team) %>% 
  summarise(team.corners_elo = last(team.corners_elo))

current_corners_elo_ratings %>%  arrange(desc(team.corners_elo))

current_corners_elo_ratings_lag <-
  rbind(
    data.frame(
      match_id = corners_elo$match_id,
      team = corners_elo$home.team, 
      team.corners_elo = corners_elo$updated.home.rating),
    data.frame(
      match_id = corners_elo$match_id,
      team = corners_elo$away.team, 
      team.corners_elo = corners_elo$updated.away.rating)) %>% 
  
  arrange(desc(match_id)) %>% 
  ungroup() %>% 
  select(team, team.corners_elo) %>% 
  group_by(team) %>% 
  filter(row_number()==3)

#### Join corners_elos to Match Level Data

corners_elo_output <- cbind(corners_elo[1:9]) %>% 
  rename(home.corners_elo = home.rating,
         away.corners_elo = away.rating,
         home.corners_elo.prob = home.prob,
         away.corners_elo.prob = away.prob)

aleague_match_data <- left_join(aleague_match_data, corners_elo_output, by = c("season", "round", "home.team",
                                                                       "away.team", "match_id"))

aleague_match_data$corners_elo.diff <- aleague_match_data$home.corners_elo - aleague_match_data$away.corners_elo


aleague_match_data <- aleague_match_data %>% 
  dplyr::mutate(corners_elo.diff.cat = ifelse(corners_elo.diff > k_val, "Much Stronger Home",
                                      ifelse(corners_elo.diff < -k_val, "Much Stronger Away", 
                                             ifelse(corners_elo.diff > 0, "Stronger Home", "Stronger Away"))))


#### Join corners_elos to Team Level Data
corners_elo_ratings_teams <-
  rbind(
    data.frame(
      match_id = corners_elo$match_id,
      team = corners_elo$home.team, 
      corners_elo = corners_elo$home.rating,
      corners_elo.prob = corners_elo$home.prob), 
    data.frame(
      match_id = corners_elo$match_id,
      team = corners_elo$away.team, 
      corners_elo = corners_elo$away.rating,
      corners_elo.prob = corners_elo$away.prob)) %>% 
  arrange(match_id) %>% 
  ungroup() %>% 
  group_by(team)

aleague_team_opp_data <- left_join(aleague_team_opp_data, corners_elo_ratings_teams, by = c('match_id' = 'match_id', 'team' = 'team')) %>% 
  dplyr::rename(team.corners_elo = corners_elo,
                team.corners_elo.prob = corners_elo.prob) 

aleague_team_opp_data <- left_join(aleague_team_opp_data, corners_elo_ratings_teams, by = c('match_id' = 'match_id', 'opp.team' = 'team')) %>% 
  dplyr::rename(opp.team.corners_elo = corners_elo,
                opp.team.corners_elo.prob = corners_elo.prob) %>% 
  dplyr::mutate(corners_elo.diff = team.corners_elo- opp.team.corners_elo)


aleague_team_opp_data <- aleague_team_opp_data %>% 
  dplyr::mutate(corners_elo.diff.cat = ifelse(corners_elo.diff > k_val, "Much Stronger Team",
                                      ifelse(corners_elo.diff < -k_val, "Much Stronger Opp", 
                                             ifelse(corners_elo.diff > 0, "Stronger Team", "Stronger Opp"))))


#### Join to Predictions

current_corners_elo_ratings_home <- current_corners_elo_ratings %>% rename(home.team.corners_elo = team.corners_elo)
current_corners_elo_ratings_away <- current_corners_elo_ratings %>% rename(away.team.corners_elo = team.corners_elo)

aleague_predictions_data <- left_join(aleague_predictions_data, current_corners_elo_ratings_home, by = c('home.team' = 'team'))
aleague_predictions_data <- left_join(aleague_predictions_data, current_corners_elo_ratings_away, by = c('away.team' = 'team'))

aleague_predictions_data$no.crowd.factor = ifelse(aleague_predictions_data$season == 2020 & aleague_predictions_data$round.number > 1,2,1)

aleague_predictions_data$corners_elo.diff <- aleague_predictions_data$home.team.corners_elo - aleague_predictions_data$away.team.corners_elo

aleague_predictions_data$home.corners_elo.prob <- 1 / (1 + 10^((-(aleague_predictions_data$corners_elo.diff))/400))
aleague_predictions_data$away.corners_elo.prob <- 1 - aleague_predictions_data$home.corners_elo.prob

aleague_predictions_data <- aleague_predictions_data %>% 
  dplyr::mutate(corners_elo.diff.cat = ifelse(corners_elo.diff > k_val, "Much Stronger Team",
                                      ifelse(corners_elo.diff < -k_val, "Much Stronger Opp", 
                                             ifelse(corners_elo.diff > 0, "Stronger Team", "Stronger Opp"))))



#### Map Elos to Totals ####




