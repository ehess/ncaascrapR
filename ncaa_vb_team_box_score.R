
ncaa_vb_team_box_score <- function(game_id) {
  base_url <- "https://data.ncaa.com/casablanca/game/"
  
  full_url <- paste(base_url, game_id, "boxscore.json", sep="/")
  
  # Check for internet
  #check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  #check_status(res)
  
  bs.json <- jsonlite::fromJSON(full_url, flatten = TRUE)
  
  bs.df <- as.data.frame(bs.json$teams)
  bs.meta <- as.data.frame(bs.json$meta$teams)
  
  bs.team <- bs.df %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$sets, names_repair = "unique") %>%
    dplyr::select(teamId, setNumber, kills, attackErrors, hittingPercentage, attackAttempts)
  
  bs.final <- base::merge(bs.team, bs.meta, by.x = "teamId", by.y = "id")
  
  return(bs.team)
}
