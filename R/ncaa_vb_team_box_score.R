#' @title ncaa_vb_team_box_score
#' @description Scrape team box score during a volleyball game from ncaa.com data
#'
#' @details How to get a game ID:
#' 1. find the game you want to view data for via ncaa.com's scoreboard
#' 2. open up its box score page
#' 3. Copy the string of numbers at the end of the URL from your browser's address bar.
#' OR
#' 1. call the ncaa_vb_games() function
#' 2. Select the game_id for the given game
#'
#' @param game_id the ID for the game's box score data
#' @returns a data frame of team box score data for the specified game
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

  return(bs.final)
}
