
ncaa_vb_player_box_score <- function(game_id) {
  base_url <- "https://data.ncaa.com/casablanca/game/"
  
  full_url <- paste(base_url, game_id, "boxscore.json", sep="/")
  
  # Check for internet
  #check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  #check_status(res)
  
  bs.json <- jsonlite::fromJSON(full_url, flatten = TRUE)
  
  bs.df <- base::as.data.frame(bs.json$teams)
  
  player.data <- bs.df %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    dplyr::select(teamId, playerStats) %>%
    tidyr::unnest(.data$playerStats, names_repair = "unique") %>%
    base::unique(.)
  
  return(player.data)
}
