#TODO: can we use starter info, and sub info to determine who was on the court for every point?
#TODO: collapse plays into single scoring events (column for scoring text, home subs, away subs)
#TODO: ignore warnings on the score string split
ncaa_vb_pbp <- function(game_id) {
  base_url <- "https://data.ncaa.com/casablanca/game/"
  
  full_url <- paste(base_url, game_id, "pbp.json", sep="/")
  
  # Check for internet
  #check_internet()
  
  # Create the GET request and set response as res
  res <- httr::GET(full_url)
  
  # Check the result
  #check_status(res)
  
  pbp.json <- jsonlite::fromJSON(full_url, flatten = TRUE)
  
  pbp.df <- as.data.frame(pbp.json$periods)
  
  pbp <- pbp.df %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$playStats) %>%
    tidyr::separate(.data$score, c("visitorScore", "homeScore")) %>%
    dplyr::mutate(playType = ifelse(!is.na(.data$homeScore), "Scoring", "Non-Scoring"),
                  matchPlayNumber = row_number()) %>%
    dplyr::group_by(periodNumber) %>%
    dplyr::mutate(setPlayNumber = row_number())
  
  pbp[,3:4] <- sapply(pbp[,3:4],as.numeric)
  
  return(pbp)
}
