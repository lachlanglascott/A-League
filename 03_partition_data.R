#### Datasets ####

data_test <- data %>% filter(datetime > "2020-01-12T18:00:00+11:00")

test_matches <- data_test %>% select(match_id) %>% distinct() %>% as.matrix()
'%notin%' <- Negate('%in%')

data_train <- data %>% filter(match_id %notin% test_matches)

data_predictions_home <- data_test %>% filter(home == "Home") %>% group_by(match_id, season, round, datetime, venue, team) %>% summarise(home.corners_taken = sum(stats.corners_taken),  home.corners_won = sum(stats.corners_won)) %>% rename(home.team = team)
data_predictions_away <- data_test %>% filter(home != "Home") %>% group_by(match_id, season, round, datetime, venue, team) %>% summarise(away.corners_taken = sum(stats.corners_taken),  away.corners_won = sum(stats.corners_won)) %>% rename(away.team = team)

data_predictions <- left_join(data_predictions_home, data_predictions_away, by = c("match_id", "season", "round","datetime", "venue"))


data_test <- data_test %>% select(-contains("stats."))

#write.csv(data_train, file = "C:/Users/glascottl/OneDrive - Tabcorp/Documents/R/Model data/aleague/aleague_train.csv")
#write.csv(data_test, file = "C:/Users/glascottl/OneDrive - Tabcorp/Documents/R/Model data/aleague/aleague_test.csv")
#write.csv(data_predictions, file = "C:/Users/glascottl/OneDrive - Tabcorp/Documents/R/Model data/aleague/aleague_predictions_data.csv")


data_train %>% 
  summarise(n_distinct(match_id))

data_test %>% 
  summarise(n_distinct(match_id))

rounds <- data %>% filter(season == 2020) %>% 
  group_by(round) %>% 
  summarise(max(datetime))

