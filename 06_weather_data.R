#### Set Cities/States ####
capital_city_coordinates <- as.data.frame(read_excel("C:/Users/glascottl/OneDrive - Tabcorp/Documents/R/Model data/nrl/NRL_venues.xlsx")) %>% 
  dplyr::select(-venue, -lat, -lon) %>% 
  filter((town %in% c("Sydney") & closest_obs_name %in% c("SYDNEY (OBSERVATORY HILL)")) | town %in% c("Melbourne", "Canberra", "Adelaide", "Brisbane", "Darwin", "Alice Springs", "Perth", "Wellington")) %>% 
  distinct()

state <- unique(capital_city_coordinates$state)
numstates <- length(capital_city_coordinates)
capital_city_coordinates$id <- seq.int(nrow(capital_city_coordinates))

#### Rainfall Data ####
rain_df <- data.frame(names = character(0),facts = character(0),nm = character(0))

#### First rain loop
for(i in 1:numstates){
  state <- capital_city_coordinates %>%
    dplyr::filter(id == i) 
  
  rain_data <- get_historical(latlon = c(state$closest_lat, state$closest_lon), type = "rain") %>% 
    dplyr::filter(year %in% c("2015","2016", "2017", "2018", "2019", "2020"))
  
  rain_data$lat <- state$lat
  rain_data$lon <- state$lon
  rain_data$id <- i
  rain_data$state <- state$state
  rain_data$obs <- 1
  rain_df <- rbind(rain_df, rain_data)
}

#### Second rain loop
rain_df2 <- data.frame(names = character(0),facts = character(0),nm = character(0))

for(i in 1:numstates){
  state <- capital_city_coordinates %>%
    dplyr::filter(id == i) 
  
  rain_data2 <- get_historical(latlon = c(state$second_lat, state$second_lon), type = "rain") %>% 
    dplyr::filter(year %in% c("2015","2016", "2017", "2018", "2019", "2020"))
  
  rain_data2$lat <- state$lat
  rain_data2$lon <- state$lon
  rain_data2$id <- i
  rain_data2$state <- state$state
  rain_data2$obs <- 2
  rain_df2 <- rbind(rain_df2, rain_data2)
}

#### Max Temperature
temp_df <- data.frame(names = character(0),facts = character(0),nm = character(0))

for(i in 1:numstates){
  state <- capital_city_coordinates %>%
    dplyr::filter(id == i) 
  
  temp_data <- get_historical(latlon = c(state$closest_lat, state$closest_lon), type = "max") %>% 
    dplyr::filter(year %in% c("2015","2016", "2017", "2018", "2019", "2020"))
  
  temp_data$lat <- state$lat
  temp_data$lon <- state$lon
  temp_data$id <- i
  temp_data$state <- state$state
  temp_data$obs <- 1
  temp_df <- rbind(temp_df, temp_data)
}

#### Second temp loop
temp_df2 <- data.frame(names = character(0),facts = character(0),nm = character(0))

for(i in 1:numstates){
  state <- capital_city_coordinates %>%
    dplyr::filter(id == i) 
  
  temp_data2 <- get_historical(latlon = c(state$second_lat, state$second_lon), type = "max") %>% 
    dplyr::filter(year %in% c("2015", "2016", "2017", "2018", "2019", "2020"))
  
  temp_data2$lat <- state$lat
  temp_data2$lon <- state$lon
  temp_data2$id <- i
  temp_data2$state <- state$state
  temp_data2$obs <- 2
  temp_df2 <- rbind(temp_df2, temp_data2)
}



#make this an rbind when working
rain_df <- rbind(rain_df, rain_df2)
temp_df <- rbind(temp_df, temp_df2)
write.csv(rain_df, file = "rain_df.csv")
write.csv(temp_df, file = "temp_df.csv")

rain_df <- read.csv("rain_df.csv")

# Fix dates
rain_df$date <- with(rain_df, ymd(paste(year, month, day, sep= ' ')))
temp_df$date <- with(temp_df, ymd(paste(year, month, day, sep= ' ')))

# Join 
rain_data_closest <- rain_df %>% 
  dplyr::filter(obs == 1) %>% 
  dplyr::select(state, date, rainfall) %>% 
  dplyr::mutate(count = n()) %>% 
  distinct()

rain_data_second <- rain_df %>% 
  dplyr::filter(obs == 2) %>% 
  dplyr::select(state, date, rainfall) %>% 
  dplyr::mutate(count = n()) %>% 
  dplyr::rename(rainfall2 = rainfall) %>% 
  distinct()

temp_data_closest <- temp_df %>% 
  dplyr::filter(obs == 1) %>% 
  dplyr::select(state, date, max_temperature) %>% 
  dplyr::mutate(count = n()) %>% 
  distinct()

temp_data_second <- temp_df %>% 
  dplyr::filter(obs == 2) %>% 
  dplyr::select(state, date, max_temperature) %>% 
  dplyr::mutate(count = n()) %>% 
  dplyr::rename(max_temperature2 = max_temperature) %>% 
  distinct()

weather_df <- temp_data_closest %>% 
  left_join(rain_data_closest, by = c("state", "date")) %>% 
  left_join(temp_data_second, by = c("state", "date")) %>% 
  left_join(rain_data_second, by = c("state", "date")) %>% 
  dplyr::mutate(rainfall = ifelse(is.na(rainfall) == TRUE, rainfall2, rainfall),
                max_temperature = ifelse(is.na(max_temperature) == TRUE, max_temperature2, max_temperature)) %>% 
  dplyr::select(date, state, rainfall, max_temperature) %>% 
  mutate(rainfall = ifelse(is.na(rainfall) == TRUE, 0, rainfall),
         max_temperature = ifelse(is.na(max_temperature), 25, max_temperature)) %>%
  dplyr::mutate(rainfall.cat = ifelse(rainfall < 2, "Low Probability",
                                           ifelse(rainfall < 10, "Medium Probability",
                                                  ifelse(rainfall >= 10, "High Probability", "NA"))),
                max_temperature.cat = ifelse(max_temperature < 20, "Cool",
                                           ifelse(max_temperature >= 20 & max_temperature  < 30, "Warm",
                                                  ifelse(max_temperature  >= 30, "Hot", "NA"))))
                

#### Join Data ####

# Match Dataset
aleague_match_data <- left_join(aleague_match_data, weather_df, by = c("state" = "state", "date" = "date")) 

aleague_match_data <- aleague_match_data %>% 
  mutate(rainfall = ifelse(is.na(rainfall) == TRUE, 0, rainfall),
       max_temperature = ifelse(is.na(max_temperature), 25, max_temperature)) 

# Team Dataset
aleague_team_opp_data <- left_join(aleague_team_opp_data, weather_df, by = c("state" = "state", "date" = "date")) 

aleague_team_opp_data <- aleague_team_opp_data %>% 
  mutate(rainfall = ifelse(is.na(rainfall) == TRUE, 0, rainfall),
         max_temperature = ifelse(is.na(max_temperature), 25, max_temperature)) 

# Precictions Dataset
aleague_predictions_data <- left_join(aleague_predictions_data, weather_df, by = c("state" = "state", "date" = "date")) 

aleague_predictions_data <- aleague_predictions_data %>% 
  mutate(rainfall = ifelse(is.na(rainfall) == TRUE, 0, rainfall),
         max_temperature = ifelse(is.na(max_temperature), 25, max_temperature)) 

#### See weather forecast below for new weeks

#### Forecast ####
# weather_forecast <- get_precis_forecast()
# 
# weather_forecast[is.na(weather_forecast)] <- 0
# weather_forecast$date <- as.Date(weather_forecast$end_time_local)
# 
# weather_forecast_df <- weather_forecast %>% 
#   dplyr::select(town, date, upper_precipitation_limit, maximum_temperature) %>% 
#   dplyr::mutate(town = ifelse(town == "Gold Coast Seaway", "Gold Coast", town)) %>% 
#   dplyr::rename(city = town, rainfall = upper_precipitation_limit, max_temperature = maximum_temperature) 
# 
# aleague_predictions_data <- left_join(aleague_predictions_data, weather_forecast_df, by = c("city" = "city", "date" = "date")) %>% 
#   mutate(rainfall = ifelse(is.na(rainfall) == TRUE, 0, rainfall),
#          max_temperature = ifelse(is.na(max_temperature), 25, max_temperature)) %>%
#   dplyr::mutate(rainfall.cat = ifelse(rainfall < 2, "Low Probability",
#                                       ifelse(rainfall < 10, "Medium Probability",
#                                              ifelse(rainfall >= 10, "High Probability", "NA"))),
#                 max_temperature.cat = ifelse(max_temperature < 20, "Cool",
#                                              ifelse(max_temperature >= 20 & max_temperature  < 30, "Warm",
#                                                     ifelse(max_temperature  >= 30, "Hot", "NA")))) 

