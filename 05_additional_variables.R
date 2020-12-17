#### Add Extra Variabvles to final Datasets ####

#### Win & Margin ####

# Match Level Data
aleague_match_data$home.goals_margin <- aleague_match_data$home.stats.goals - aleague_match_data$away.stats.goals
aleague_match_data$home.win <- ifelse(aleague_match_data$home.stats.goals - aleague_match_data$away.stats.goals > 0, 1, 0)

aleague_match_data$home.corners_margin <- aleague_match_data$home.stats.corners_taken - aleague_match_data$away.stats.corners_taken
aleague_match_data$home.corners_win <- ifelse(aleague_match_data$home.stats.corners_taken - aleague_match_data$away.stats.corners_taken > 0, 1, 0)

# Team Level Data
aleague_team_opp_data$team.goals_margin <- aleague_team_opp_data$team.stats.goals - aleague_team_opp_data$opp.stats.goals
aleague_team_opp_data$team.win <- ifelse(aleague_team_opp_data$team.stats.goals - aleague_team_opp_data$opp.stats.goals > 0, 1, 0)

aleague_team_opp_data$corners_margin <- aleague_team_opp_data$team.stats.corners_taken - aleague_team_opp_data$opp.stats.corners_taken
aleague_team_opp_data$team.corners_win <- ifelse(aleague_team_opp_data$team.stats.corners_taken - aleague_team_opp_data$opp.stats.corners_taken > 0, 1, 0)

# Predictions Level Data
#aleague_team_opp_data$team.goals_margin <- aleague_team_opp_data$team.stats.goals - aleague_team_opp_data$opp.stats.goals
#aleague_team_opp_data$team.win <- ifelse(aleague_team_opp_data$team.stats.goals - aleague_team_opp_data$opp.stats.goals > 0, 1, 0)

#aleague_team_opp_data$corners_margin <- aleague_team_opp_data$team.stats.corners_taken - aleague_team_opp_data$opp.stats.corners_taken
#aleague_team_opp_data$team.corners_win <- ifelse(aleague_team_opp_data$team.stats.corners_taken - aleague_team_opp_data$opp.stats.corners_taken > 0, 1, 0)

#### State ####

# Match Dataset
aleague_match_data <- aleague_match_data %>% 
  mutate(state = case_when(city %in% c("Sydney", "Newcastle", "Gosford", "Central Coast", "Mudgee", "Coffs Harbour") ~ "NSW",
                           city %in% c("Melbourne", "Geelong", "Ballarat", "Morwell") ~ "VIC",
                           city %in% c("Brisbane", "Robina", "Gold Coast", "Townsville") ~ "QLD",
                           city %in% c("Adelaide") ~ "SA",
                           city %in% c("Perth") ~ "WA", 
                           city %in% c("Hamilton", "Auckland", "Wellington", "Christchurch", "New Plymouth") ~ "NZL",
                           city %in% c("Cairns", "Darwin") ~ "NT",
                           city %in% c("Canberra") ~ "ACT",
                           city %in% c("Hobart", "Launceston") ~ "TAS"))

# Team Dataset
aleague_team_opp_data <- aleague_team_opp_data %>% 
  mutate(state = case_when(city %in% c("Sydney", "Newcastle", "Gosford", "Central Coast", "Mudgee", "Coffs Harbour") ~ "NSW",
                           city %in% c("Melbourne", "Geelong", "Ballarat", "Morwell") ~ "VIC",
                           city %in% c("Brisbane", "Robina", "Gold Coast", "Townsville") ~ "QLD",
                           city %in% c("Adelaide") ~ "SA",
                           city %in% c("Perth") ~ "WA", 
                           city %in% c("Hamilton", "Auckland", "Wellington", "Christchurch", "New Plymouth") ~ "NZL",
                           city %in% c("Cairns", "Darwin") ~ "NT",
                           city %in% c("Canberra") ~ "ACT",
                           city %in% c("Hobart", "Launceston") ~ "TAS"))

# Precictions Dataset
aleague_predictions_data <- aleague_predictions_data %>% 
  mutate(state = case_when(city %in% c("Sydney", "Newcastle", "Gosford", "Central Coast", "Mudgee", "Coffs Harbour") ~ "NSW",
                           city %in% c("Melbourne", "Geelong", "Ballarat", "Morwell") ~ "VIC",
                           city %in% c("Brisbane", "Robina", "Gold Coast", "Townsville") ~ "QLD",
                           city %in% c("Adelaide") ~ "SA",
                           city %in% c("Perth") ~ "WA", 
                           city %in% c("Hamilton", "Auckland", "Wellington", "Christchurch", "New Plymouth") ~ "NZL",
                           city %in% c("Cairns", "Darwin") ~ "NT",
                           city %in% c("Canberra") ~ "ACT",
                           city %in% c("Hobart", "Launceston") ~ "TAS"))


#### Round Band ####

# Match Dataset
aleague_match_data$month <- as.character(aleague_match_data$month)
aleague_match_data <- aleague_match_data %>% 
  mutate(round_band = case_when(round.number %in% 1:5 ~ '1-5',
                                round.number %in% 6:10 ~ '6-10',
                                round.number %in% 11:15 ~ '11-15',
                                round.number %in% 16:20 ~ '16-20',
                                round.number >20 ~ '20+',
                                TRUE ~ 'Other'
  ))

# Team Dataset
aleague_team_opp_data$month <- as.character(aleague_team_opp_data$month)
aleague_team_opp_data <- aleague_team_opp_data %>% 
  mutate(round_band = case_when(round.number %in% 1:5 ~ '1-5',
                                round.number %in% 6:10 ~ '6-10',
                                round.number %in% 11:15 ~ '11-15',
                                round.number %in% 16:20 ~ '16-20',
                                round.number >20 ~ '20+',
                                TRUE ~ 'Other'
  ))


# Precictions Dataset
aleague_predictions_data$month <- as.character(aleague_predictions_data$month)
aleague_predictions_data <- aleague_predictions_data %>% 
  mutate(round_band = case_when(round.number %in% 1:5 ~ '1-5',
                                round.number %in% 6:10 ~ '6-10',
                                round.number %in% 11:15 ~ '11-15',
                                round.number %in% 16:20 ~ '16-20',
                                round.number >20 ~ '20+',
                                TRUE ~ 'Other'
  ))



#### Goals & Corners Band ####

# Match Dataset
aleague_match_data <- aleague_match_data %>% 
  dplyr::mutate(total.goals = home.stats.goals + away.stats.goals,
                total.goals_band = case_when(total.goals <= 2  ~ '0-2',
                                                     total.goals > 2 & total.goals <= 4 ~ '3-4',
                                                     total.goals > 4  ~ '4+'),
                total.corners_taken = home.stats.corners_taken + away.stats.corners_taken,
                total.corners_taken_band = case_when(total.corners_taken <= 9  ~ '0-2',
                                                       total.corners_taken > 9 & total.corners_taken <= 12 ~ '3-4',
                                                       total.corners_taken > 12  ~ '4+'))

                
                
# Team Dataset
aleague_team_opp_data <- aleague_team_opp_data %>% 
  dplyr::mutate(total.goals = team.stats.goals + opp.stats.goals,
                total.goals_band = case_when(total.goals <= 2  ~ '0-2',
                                             total.goals > 2 & total.goals <= 4 ~ '3-4',
                                             total.goals > 4  ~ '4+'),
                total.corners_taken = team.stats.corners_taken + opp.stats.corners_taken,
                total.corners_taken_band = case_when(total.corners_taken <= 9  ~ '0-2',
                                               total.corners_taken > 9 & total.corners_taken <= 12 ~ '3-4',
                                               total.corners_taken > 12  ~ '4+'))




# Precictions Dataset
#Calculated in 04_poisson_model



#### Estimated minutes trailing in match ####

# Match Dataset

aleague_match_data <- aleague_match_data %>% 
  mutate(home.est_mins_trailing = case_when(home.stats.goals-away.stats.goals == -1 ~ 30,
                                                 home.stats.goals-away.stats.goals <= -2 ~ 45,
                                                 TRUE ~ 0),
         away.est_mins_trailing = case_when(away.stats.goals-home.stats.goals == -1 ~ 30,
                                                 away.stats.goals-home.stats.goals <= -2 ~ 45,
                                                 TRUE ~ 0))


# Team Dataset
aleague_team_opp_data <- aleague_team_opp_data %>% 
  mutate(team.est_mins_trailing = case_when(team.stats.goals-opp.stats.goals == -1 ~ 30,
                                                 team.stats.goals-opp.stats.goals <= -2 ~ 45,
                                                 TRUE ~ 0),
         opp.est_mins_trailing = case_when(opp.stats.goals-team.stats.goals == -1 ~ 30,
                                                 opp.stats.goals-team.stats.goals <= -2 ~ 45,
                                                 TRUE ~ 0))



# Predictions Dataset
#Calculated in 04_poisson_model










