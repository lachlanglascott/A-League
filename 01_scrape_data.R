#### Load 2021 Data ####
#load functions
flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}


dateurl <- 'https://statsapi.foxsports.com.au/3.0/api/sports/league/matches/aleague19190904/scoreboard.json;masthead=foxsports?userkey=A00239D3-45F6-4A0A-810C-54A347F144C2'
url <- 'https://statsapi.foxsports.com.au/3.0/api/sports/league/matches/aleague19190904/playerstats.json?userkey=A00239D3-45F6-4A0A-810C-54A347F144C2'

x <- GET("https://statsapi.foxsports.com.au/3.0/api/sports/league/matches/aleague19190904/scoreboard.json;masthead=foxsports?userkey=A00239D3-45F6-4A0A-810C-54A347F144C2")

games <- c(150101:150108,
           150201:150208,
           150301:150308, 
           150401:150408,
           150501:150508, 
           150601:150608,
           150701:150708, 
           150801:150808,
           150901:150908, 
           151001:151008,
           151101:151108, 
           151201:151208,
           151301:151308, 
           151401:151408,
           151501:151508, 
           151601:151608,
           151701:151708, 
           151801:151808,
           151901:151908, 
           152001:152008,
           152101:152108, 
           152201:152208,
           152301:152308, 
           152401:152408,
           152501:152508, 
           152601:152608,
           152701:152708,
           152801:152808, 
           152901:152908,
           153001:153008, 
           153101:153108
)

games <- c(19190101:19190108)

season <- 2019

df <- data.frame(names = character(0),facts = character(0),nm = character(0))

for(i in games){
  
  
  dateurl <- paste0("https://statsapi.foxsports.com.au/3.0/api/sports/football/matches/HAL2018-",i,"/scoreboard.json;masthead=foxsports?userkey=A00239D3-45F6-4A0A-810C-54A347F144C2")
  
  #ERROR HANDLING
  possibleError <- tryCatch(
    http_status(GET(dateurl))$category,
    error=function(e) e
  )
  
  if(possibleError == "Client error") next
  
  #REAL WORK
  
  dateurl <- paste0("https://statsapi.foxsports.com.au/3.0/api/sports/football/matches/HAL2018-",i,"/scoreboard.json;masthead=foxsports?userkey=A00239D3-45F6-4A0A-810C-54A347F144C2")
  
  datejson <- fromJSON(dateurl)
  
  datelist <- flattenlist(datejson)
  
  season <- datelist[["season.year"]]
  date <- datelist[["match_start_date"]]
  round <- datelist[["round.name"]]
  venue <- datelist[["venue.name"]]
  city <- datelist[["venue.city"]]
  
  url <- paste0("https://statsapi.foxsports.com.au/3.0/api/sports/football/matches/HAL2019-",i,"/playerstats.json?userkey=A00239D3-45F6-4A0A-810C-54A347F144C2")
  
  json <- fromJSON(url)
  
  list <- flattenlist(json)
  
  home_data <- as.data.frame(as.matrix(list[["team_A.players"]]))
  home_data$team <- list[["team_A.name"]]
  home_data$home <- "Home"
  
  away_data <- as.data.frame(as.matrix(list[["team_B.players"]]))
  away_data$team <- list[["team_B.name"]]
  away_data$home <- "Away"
  
  data <- rbind(home_data, away_data)
  
  data$match_id <- i
  data$season <- season
  data$datetime <- date
  data$round <- round
  data$venue <- venue
  data$city <- city
  
  df <- rbind(df, data)
  print(data$match_id)
  Sys.sleep(1)
  
  
}

write.csv(df, file = "C:/Users/glascottl/OneDrive - Tabcorp/Documents/R/Model data/aleague/aleague2020.csv")

aleague_data <- aleague_historical
aleague_data <- bind_rows(aleague2020, aleague_historical)