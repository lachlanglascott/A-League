



#### Player Data ####
aleague_player_data <- data_train 

#### Team Data ####
aleague_team_data <- data_train

#### Additional Match Description Data ####
aleague_team_data$match_id <- as.character(aleague_team_data$match_id)
aleague_team_data$team <- as.character(aleague_team_data$team)
aleague_team_data$time <- substring(aleague_team_data$datetime,12,19)
aleague_team_data$hour <- as.numeric(substring(aleague_team_data$datetime,12,13))
aleague_team_data$month <- as.numeric(substring(aleague_team_data$datetime,6,7))
aleague_team_data$day.night <- ifelse((aleague_team_data$city == "Perth") & (aleague_team_data$hour > 21), "Night", 
                                      ifelse((aleague_team_data$city != "Perth") & (aleague_team_data$hour > 18), "Night", "Day"))
aleague_team_data$date <- as.Date(substring(aleague_team_data$datetime,0,10))
aleague_team_data$season <- as.numeric(aleague_team_data$season) + 1
aleague_team_data$season.round <- paste0(aleague_team_data$season, aleague_team_data$round)
aleague_team_data$round.number <- as.numeric(substring(aleague_team_data$round,7,8))

match_vars_2 <- c(match_vars_1, "time", "hour", "month", "day.night", "date", "season.round", "round.number")

#### Team Level Data ####

aleague_team_data <- aleague_team_data %>% 
  group_by_at(match_vars_2) %>%
  summarise_at(stats_vars, sum) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  #fix percentage variables
  mutate(stats.aerial_contests_won_percentage = stats.aerial_contests_won/ stats.aerial_contests_attempted,
         stats.attacking_third_passes_percentage =  stats.attacking_third_passes_complete/stats.attacking_third_passes,
         stats.contests_won_percentage = stats.contests_won/stats.contests_attempted, 
         stats.passes_completed = as.integer(stats.passes_completed), 
         stats.crosses_completed = as.integer(stats.crosses_completed),
         stats.pass_accuracy = stats.passes_completed / stats.passes, 
         stats.cross_accuracy = stats.crosses_completed / stats.crosses)

#### Date Weights

DCweights <- function(Date, currentDate=Sys.Date(), xi=0.003){
  datediffs <- Date - as.Date(currentDate)
  datediffs <- as.numeric(datediffs *-1)
  w <- exp(-1*xi*datediffs)
  w[datediffs <= 0] <- 0 #Future dates should have zero weights
  return(w)
}

aleague_team_data$date.weight <- DCweights(aleague_team_data$date)


#### Home Away Match Level Data ####

aleague_team_data_home <- aleague_team_data %>% 
  dplyr::filter(home == "Home")

names(aleague_team_data_home) <- paste0("home.", names(aleague_team_data_home))

aleague_team_data_home <- aleague_team_data_home %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(match_id = home.match_id,
                season = home.season,
                season.round = home.season.round,
                round = home.round,
                round.number = home.round.number,
                month = home.month, 
                date = home.date,
                datetime = home.datetime,
                date.weight = home.date.weight,
                time = home.time,
                hour = home.hour,
                day.night = home.day.night,
                venue = home.venue,
                city = home.city) %>% 
  select(-home.home)

aleague_team_data_away <- aleague_team_data %>% 
  dplyr::filter(home == "Away")

names(aleague_team_data_away) <- paste0("away.", names(aleague_team_data_away))

aleague_team_data_away <- aleague_team_data_away %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(match_id = away.match_id,
                season = away.season,
                round = away.round,
                round.number = away.round.number,
                season.round = away.season.round,
                month = away.month, 
                date = away.date,
                datetime = away.datetime,
                date.weight = away.date.weight,
                time = away.time,
                hour = away.hour,
                day.night = away.day.night,
                venue = away.venue,
                city = away.city) %>% 
  select(-away.home)

aleague_match_data <- left_join(aleague_team_data_home, aleague_team_data_away,  by = c("match_id", "season", "round", "round.number", "season.round", "date", "datetime", "date.weight", "month", "time", "hour", "day.night", "venue", "city")) 

#### Team Stats with Opposition Variables ####

aleague_team_data_home_as_opp <- aleague_team_data_home
aleague_team_data_home_as_opp$home <- "Away"
names(aleague_team_data_home_as_opp) <- gsub(x = names(aleague_team_data_home_as_opp), pattern = "home.", replacement = "opp.") 

#away
aleague_team_data_away_as_opp <- aleague_team_data_away
aleague_team_data_away_as_opp$home <- "Home"
names(aleague_team_data_away_as_opp) <- gsub(x = names(aleague_team_data_away_as_opp), pattern = "away.", replacement = "opp.")

aleague_team_data_home_as_opp_join <- aleague_team_data_home_as_opp %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-date, -season, -round, -round.number, -season.round, -date, -month, -time, -day.night, -venue, -city, -date.weight, -datetime, -hour)

aleague_team_data_away_as_opp_join <- aleague_team_data_away_as_opp %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-date, -season, -round, -round.number, -season.round, -date, -month, -time, -day.night, -venue, -city, -date.weight, -datetime, -hour)

aleague_team_data_home_opp <- left_join(aleague_team_data_home, aleague_team_data_away_as_opp_join,  by = c("match_id"))
names(aleague_team_data_home_opp) <- gsub(x = names(aleague_team_data_home_opp), pattern = "home.", replacement = "team.")

#away
aleague_team_data_away_opp <- left_join(aleague_team_data_away, aleague_team_data_home_as_opp_join,  by = c("match_id"))
names(aleague_team_data_away_opp) <- gsub(x = names(aleague_team_data_away_opp), pattern = "away.", replacement = "team.") 

#now rbind
aleague_team_opp_data <- rbind(aleague_team_data_away_opp, aleague_team_data_home_opp) %>% 
  rename(team = team.team)


#### List of Unique Venues ####
unique_venues <- aleague_team_opp_data %>% 
  dplyr::select(venue, city) %>% 
  dplyr::mutate(state = case_when(city %in% c("Sydney", "Newcastle", "Gosford", "Central Coast", "Mudgee", "Coffs Harbour") ~ "NSW",
                                  city %in% c("Melbourne", "Geelong", "Ballarat", "Morwell") ~ "VIC",
                                  city %in% c("Brisbane", "Robina", "Gold Coast", "Townsville") ~ "QLD",
                                  city %in% c("Adelaide") ~ "SA",
                                  city %in% c("Perth") ~ "WA", 
                                  city %in% c("Hamilton", "Auckland", "Wellington", "Christchurch", "New Plymouth") ~ "NZL",
                                  city %in% c("Cairns", "Darwin") ~ "NT",
                                  city %in% c("Canberra") ~ "ACT",
                                  city %in% c("Hobart", "Launceston") ~ "TAS")) %>% 
  distinct() 

#### Fixture ####

aleague_predictions_data <- data_predictions %>% 
  select(-home.corners_taken, -home.corners_won, -away.corners_taken, -away.corners_won) %>% 
  left_join(unique_venues, by = c("venue" = "venue"))

aleague_predictions_data$time <- substring(aleague_predictions_data$datetime,12,19)
aleague_predictions_data$hour <- as.numeric(substring(aleague_predictions_data$datetime,12,13))
aleague_predictions_data$month <- as.numeric(substring(aleague_predictions_data$datetime,6,7))
aleague_predictions_data$day.night <- ifelse((aleague_predictions_data$city == "Perth") & (aleague_predictions_data$hour > 21), "Night", 
                                      ifelse((aleague_predictions_data$city != "Perth") & (aleague_predictions_data$hour > 18), "Night", "Day"))
aleague_predictions_data$date <- as.Date(substring(aleague_predictions_data$datetime,0,10))
aleague_predictions_data$season <- as.numeric(aleague_predictions_data$season) + 1
aleague_predictions_data$season.round <- paste0(aleague_predictions_data$season, aleague_predictions_data$round)
aleague_predictions_data$round.number <- as.numeric(substring(aleague_predictions_data$round,7,8))





#### Features ####
# Team Elos
# Corner Elos
# Poisson Corners

#### Stats Based Model
# Shots Rating
# Width Rating
# Goals Rating

#### Team Elo Ratings ####






