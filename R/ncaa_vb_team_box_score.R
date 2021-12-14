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
  base_url <- "https://stats.ncaa.org/contests"

  full_url <- paste(base_url, game_id, "box_score", sep="/")

  base <- rvest::read_html(full_url)

  all_tables <- base %>%
    rvest::html_elements(".mytable") %>%
    rvest::html_table()

  team_box_score <- all_tables[[1]]

  col_names <- c("Team", paste0("Set ", seq(1, ncol(team_box_score)-2, by = 1)), "Sets Won")

  colnames(team_box_score) <- col_names

  team_box_score <- team_box_score %>%
    dplyr::filter(.data$Team != "") %>%
    as.data.frame() %>%
    janitor::clean_names()

  return(team_box_score)
}
