#' @title ncaa_vb_game_info
#' @description Scrape player box score during a volleyball game
#'
#' @details How to get a game ID:
#' 1. find the game you want to view data for via ncaa.com's scoreboard
#' 2. open up its box score page
#' 3. Copy the string of numbers at the end of the URL from your browser's address bar.
#' OR
#' 1. call the ncaa_vb_games() function
#' 2. Select the game_id for the given game
#'
#' @param game_id the ID for the game's data
#' @returns a data frame of of a single row for info about the selected game
#' @export
ncaa_vb_game_info <- function(game_id) {
  game_info <- ncaa_game_info(game_id)

  return(game_info)
}
