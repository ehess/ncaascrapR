
#' ncaa_vb_player_box_score()
#' Scrape player box score during a volleyball game from ncaa.com data
#'
#' How to get a game ID:
#' 1. find the game you want to view data for via ncaa.com's scoreboard
#' 2. open up its box score page
#' 3. Copy the string of numbers at the end of the URL from your browser's address bar.
#' OR
#' 1. call the ncaa_vb_games() function
#' 2. Select the game_id for the given game
#'
#' @param game_id the ID for the game's box score data
#' @returns a data frame of player box score data for the specified game
ncaa_vb_player_box_score <- function(game_id) {
  base_url <- "https://stats.ncaa.org/contests"

  full_url <- paste(base_url, game_id, "box_score", sep="/")

  base <- rvest::read_html(full_url)

  all_tables <- base %>%
    rvest::html_elements(".mytable") %>%
    rvest::html_table()

  team_one <- all_tables[[3]]
  team_two <- all_tables[[4]]

  col_names <- team_one[2,]

  colnames(team_one) <- col_names
  colnames(team_two) <- col_names

  team_one_final <- team_one %>%
    dplyr::mutate(Team = as.character(team_one[1,1])) %>%
    dplyr::filter(row_number() > 2 & row_number() <= nrow(team_one)-3)

  team_two_final <- team_two %>%
    dplyr::mutate(Team = team_two[1,1]) %>%
    dplyr::filter(row_number() > 2 & row_number() <= nrow(team_two)-3)

  full_box_score <- rbind(team_one_final, team_two_final)

  #Possible columns in last 10 years of box score
  cols_to_num <- c("S", "Kills", "Errors", "Svc Err", "Total Attacks", "Hit Pct", "Assists",
                   "Aces", "SErr", "Digs", "RErr", "Block Solos", "Block Assists", "BErr", "TB",
                   "PTS", "BHE", "Pct")

  #Get the columns found available for this box score
  found_cols <- colnames(full_box_score)[colnames(full_box_score) %in% cols_to_num]

  full_box_score <- full_box_score %>%
    dplyr::mutate_at(found_cols, as.numeric)

  return(full_box_score)
}
