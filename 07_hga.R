
#### Work out home grounds ####

home_ground <- aleague_match_data %>%
  dplyr::group_by(home.team, venue) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::top_n(1, n) %>% 
  dplyr::top_n(1, venue) %>% 
  left_join(unique_venues, by = c("venue" = "venue")) %>% 
  select(-n)

home_ground2 <- aleague_match_data %>%
  dplyr::group_by(home.team, venue) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::top_n(2, n) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(home.team) %>% 
  dplyr::mutate(matches = sum(n),
                perc = n/matches,
                rank = ifelse(perc<0.5, 2, 1))  %>% 
  dplyr::filter(perc > 0.1, rank == 2) %>% 
  dplyr::select(home.team, venue) %>% 
  dplyr::rename(venue.second = venue)  

home_ground <- home_ground %>% 
  left_join(home_ground2, by = c("home.team" = "home.team")) 

colnames(home_ground) <- c("team", "home.team.venue", "home.team.town", "home.team.state",  "home.team.venue.second")

home_ground_awayset <- home_ground
colnames(home_ground_awayset) <- c("team", "away.team.venue",  "away.team.town", "away.team.state","away.team.venue.second")


#add distance vars to match data
#first join table
aleague_match_data <- left_join(aleague_match_data, home_ground, by = c("home.team" = "team"))
aleague_match_data <- left_join(aleague_match_data, home_ground_awayset, by = c("away.team" = "team"))

# #now calculate distance
# aleague_match_data <- aleague_match_data  %>% 
#   dplyr::mutate(home.distance = abs(home.team.lat - lat) + abs(home.team.lon - lon)) %>% 
#   dplyr::mutate(away.distance = abs(away.team.lat - lat) + abs(away.team.lon - lon)) %>%
#   dplyr::mutate(distance.diff = away.distance - home.distance)
# 
# #now calculate experience at ground
# venue_experience_home <- aleague_match_data %>%
#   dplyr::group_by(home.team, venue) %>% 
#   dplyr::summarise(n = n())
# 
# venue_experience_away <- aleague_match_data %>%
#   dplyr::group_by(away.team, venue) %>% 
#   dplyr::summarise(n = n())
# 
# venue_experience <- left_join(venue_experience_home, venue_experience_away, by = c("home.team" = "away.team", "venue" = "venue")) %>% 
#   dplyr::rename(team = home.team, 
#                 home.games.venue = n.x, 
#                 away.games.venue = n.y) %>% 
#   dplyr::mutate(home.games.venue = ifelse(is.na(home.games.venue) == TRUE, 0, home.games.venue),
#                 away.games.venue = ifelse(is.na(away.games.venue) == TRUE, 0, away.games.venue),
#                 venue.experience = home.games.venue + away.games.venue) %>% 
#   dplyr::select(team, venue,  venue.experience)
# 
# aleague_match_data <- left_join(aleague_match_data, venue_experience, by = c("home.team" = "team", "venue" = "venue")) %>% 
#   dplyr::rename(home.venue.experience = venue.experience) %>% 
#   dplyr::mutate(home.venue.experience = ifelse(is.na(home.venue.experience) == TRUE, 0, home.venue.experience))
# 
# aleague_match_data <- left_join(aleague_match_data, venue_experience, by = c("away.team" = "team", "venue" = "venue")) %>% 
#   dplyr::rename(away.venue.experience = venue.experience) %>% 
#   dplyr::mutate(away.venue.experience = ifelse(is.na(away.venue.experience) == TRUE, 0, away.venue.experience))
# 
# #now calculate difference
# aleague_match_data <- aleague_match_data %>% 
#   dplyr::mutate(venue.experience.diff = home.venue.experience - away.venue.experience) %>% 
#   
#   #2020 adjustment
#   dplyr::mutate(venue.experience.diff = ifelse(season == 2020, venue.experience.diff/2, venue.experience.diff),
#                 distance.diff = ifelse(season == 2020, distance.diff/2, distance.diff))
# 
# #test regression for HGA variables
# 
# test <- lm(home.win ~ distance.diff + venue.experience.diff -1,
#            data=aleague_match_data)  # build linear regression model on full data
# 
# summary(test)
# 
# #HGA alpha - Weighting given to travel distance when calculating home ground advantage (HGA)
# distance.weighting <- 0.3
# 
# #HGA beta - Weighting given to ground experience when calculating HGA
# venue.experience.weighting <- 0.7
# 
# aleague_match_data <- aleague_match_data %>%
#   dplyr::mutate(home.advantage = (venue.experience.diff*venue.experience.weighting) + (distance.diff*distance.weighting))
# 
# aleague_match_data$venue.experience.diff.scale <- scale(aleague_match_data$venue.experience.diff)
# aleague_match_data$distance.diff.scale <- scale(aleague_match_data$distance.diff)

weak_ha_venues <- c("ANZ Stadium", "Allianz Stadium", "Sydney Cricket Ground", "Bankwest Stadium", "AAMI Park")

`%notin%` <- Negate(`%in%`)

aleague_match_data <- aleague_match_data %>% 
  dplyr::mutate(home.advantage.cat = case_when(
    #home venue and interstate traveller - strong
    (venue %in% c(home.team.venue, home.team.venue.second)) & (state != away.team.state) ~ 3,
    #home venue and within state traveller -medium
    (venue %in% c(home.team.venue, home.team.venue.second)) & (venue != away.team.venue | venue != away.team.venue.second) ~ 2, 
    #not home venue but right state, away travels - weak
    (venue != home.team.venue | venue != home.team.venue.second) & (venue != away.team.venue | venue != away.team.venue.second) & (state == home.team.state) & (state != away.team.state) ~ 1, 
    #home venue and away venue - none
    (venue %in% c(home.team.venue, home.team.venue.second)) & (venue %in% c(away.team.venue, away.team.venue.second)) ~ 0,
    #opposite of above
    (venue %in% c(away.team.venue, away.team.venue.second)) & (state != home.team.state) ~ -3,
    (venue %in% c(away.team.venue, away.team.venue.second)) & (venue != home.team.venue | venue != home.team.venue.second) & (state != home.team.state) ~ -2, 
    (venue != away.team.venue | venue != away.team.venue.second) & (venue != home.team.venue | venue != home.team.venue.second) & (state != home.team.state) & (state == away.team.state) ~ -1, 
    (venue != away.team.venue | venue != away.team.venue.second) & (venue != home.team.venue | venue != home.team.venue.second) & (state == home.team.state) & (state == away.team.state)  ~ 0, 
    #home venue and away venue - none
    (venue != away.team.venue | venue != away.team.venue.second) & (venue != home.team.venue | venue != home.team.venue.second) & (state != home.team.state) & (state != away.team.state) ~ 0,
    #nzl
    (home.team == "Warriors") & (state == "NZL") ~ 3 ,
    (away.team == "Warriors") & (state == "NZL") ~ -3 , 
    TRUE ~ 0)) %>% 
  dplyr::mutate(home.advantage.cat = as.numeric(home.advantage.cat)) %>% 
  dplyr::mutate(home.advantage.cat = ifelse(season == 2020 & month > 4, home.advantage.cat/2, home.advantage.cat))


aleague_match_data %>% 
  select(home.team, away.team, venue, home.advantage.cat)


#### Predictions Data ####
aleague_predictions_data <- left_join(aleague_predictions_data, home_ground, by = c("home.team" = "team"))
aleague_predictions_data <- left_join(aleague_predictions_data, home_ground_awayset, by = c("away.team" = "team"))

aleague_predictions_data <- aleague_predictions_data %>% 
  dplyr::mutate(home.advantage.cat = case_when(
    #home venue and interstate traveller - strong
    (venue %in% c(home.team.venue, home.team.venue.second)) & (state != away.team.state) ~ 3,
    #home venue and within state traveller -medium
    (venue %in% c(home.team.venue, home.team.venue.second)) & (venue != away.team.venue | venue != away.team.venue.second) ~ 2, 
    #not home venue but right state, away travels - weak
    (venue != home.team.venue | venue != home.team.venue.second) & (venue != away.team.venue | venue != away.team.venue.second) & (state == home.team.state) & (state != away.team.state) ~ 1, 
    #home venue and away venue - none
    (venue %in% c(home.team.venue, home.team.venue.second)) & (venue %in% c(away.team.venue, away.team.venue.second)) ~ 0,
    #opposite of above
    (venue %in% c(away.team.venue, away.team.venue.second)) & (state != home.team.state) ~ -3,
    (venue %in% c(away.team.venue, away.team.venue.second)) & (venue != home.team.venue | venue != home.team.venue.second) & (state != home.team.state) ~ -2, 
    (venue != away.team.venue | venue != away.team.venue.second) & (venue != home.team.venue | venue != home.team.venue.second) & (state != home.team.state) & (state == away.team.state) ~ -1, 
    (venue != away.team.venue | venue != away.team.venue.second) & (venue != home.team.venue | venue != home.team.venue.second) & (state == home.team.state) & (state == away.team.state)  ~ 0, 
    #home venue and away venue - none
    (venue != away.team.venue | venue != away.team.venue.second) & (venue != home.team.venue | venue != home.team.venue.second) & (state != home.team.state) & (state != away.team.state) ~ 0,
    #nzl
    (home.team == "Warriors") & (state == "NZL") ~ 3 ,
    (away.team == "Warriors") & (state == "NZL") ~ -3 , 
    TRUE ~ 0)) %>% 
  dplyr::mutate(home.advantage.cat = as.numeric(home.advantage.cat)) %>% 
  dplyr::mutate(home.advantage.cat = ifelse(season == 2020 & month > 4, home.advantage.cat/2, home.advantage.cat))


aleague_predictions_data %>% 
  select(home.team, away.team, venue, home.advantage.cat)

#### Team Data ####

home_advantage_vector <- aleague_match_data %>% 
  dplyr::select(match_id, home.advantage.cat) 

aleague_team_opp_data <- left_join(aleague_team_opp_data, home_advantage_vector, by = c("match_id"))

aleague_team_opp_data <- aleague_team_opp_data %>% 
  mutate(team.home.advantage.cat = ifelse(home == "Home", home.advantage.cat, -home.advantage.cat)) %>% 
  select(-home.advantage.cat)
