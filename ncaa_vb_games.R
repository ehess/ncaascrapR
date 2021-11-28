
ncaa_vb_games <- function(year, month, day) {
  base_url <- "https://data.ncaa.com/casablanca/scoreboard/volleyball-women/d1/"
  
  full_url <- paste(base_url, year, month, day, "scoreboard.json", sep="/")
  
  res <- httr::GET(full_url)
  
  # Check the result
  #check_status(res)
  
  games.json <- jsonlite::fromJSON(full_url, flatten = TRUE)
  games <- games.json$games
  
  games$AwayConference <- apply(games, 1, function(x) {
    away.conf <- unlist(x["game.away.conferences"][[1]])[1]
    return(away.conf)
  })
  
  games$HomeConference <- apply(games, 1, function(x) {
    home.conf <- unlist(x["game.home.conferences"][[1]])[1]
    return(home.conf)
  })
  
  games.final <- games %>% 
    tidyr::separate(game.url, c("empty", "game", "game.id"), "/", remove = FALSE) %>%
    dplyr::select(-c(game.away.conferences, game.home.conferences, empty, game)) %>%
    janitor::clean_names()
  
  return(games.final)
}
