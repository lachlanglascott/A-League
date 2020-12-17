###load historical data

aleague2016 <- read.csv(file = "C:/Users/glascottl/OneDrive - Tabcorp/Documents/R/Model data/aleague/aleague2016.csv")
aleague2017 <- read.csv(file = "C:/Users/glascottl/OneDrive - Tabcorp/Documents/R/Model data/aleague/aleague2017.csv")
aleague2018 <- read.csv(file = "C:/Users/glascottl/OneDrive - Tabcorp/Documents/R/Model data/aleague/aleague2018.csv")
aleague2019 <- read.csv(file = "C:/Users/glascottl/OneDrive - Tabcorp/Documents/R/Model data/aleague/aleague2019.csv")
aleague2020 <- read.csv(file = "C:/Users/glascottl/OneDrive - Tabcorp/Documents/R/Model data/aleague/aleague2020.csv")

aleague_historical <- bind_rows(aleague2016, aleague2017, aleague2018, aleague2019, aleague2020) %>% 
  dplyr::select(-X) %>% 
  rename(datetime = date) 

#### Add Any other stats variables required from base table
aleague_historical <- aleague_historical %>% 
  mutate(stats.passes_completed = (stats.pass_accuracy/100) * stats.passes,
         stats.crosses_completed = (stats.cross_accuracy/100) * stats.crosses)


#### Subset into Stats, Match, Player ####
aleague_historical_stats <- aleague_historical %>% 
  select(contains("stats.")) %>% 
  mutate_all(as.numeric)

stats_vars <- names(aleague_historical_stats)
stats_perc_vars <- c("stats.aerial_contests_won_percentage", "stats.attacking_third_passes_percentage", "stats.contests_won_percentage", "stats.pass_accuracy", "stats.cross_accuracy")

aleague_historical_match  <- aleague_historical %>% 
  select(match_id,  season, round, datetime, team, home, venue, city) %>% 
  mutate(season = season + 1) %>% 
  mutate_all(as.character)

match_vars_1 <- names(aleague_historical_match)

aleague_historical_player <- aleague_historical %>% 
  select(id, full_name, short_name, surname, other_names, date_of_birth, height_cm, weight_kg, jumper_number, position_code, position_order) 

player_vars <- names(aleague_historical_player)

data <- cbind(aleague_historical_match, aleague_historical_player, aleague_historical_stats) 
