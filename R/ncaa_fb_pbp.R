#' @title ncaa_fb_pbp
#' @description Obtains the play by play info for a NCAA FB game
#' @param game_id the game_id of a given NCAA FB game from the NCAA API
#' @return a data frame of rows for each play in the game
#' @export
#' @examples \donttest{
#'   ncaa_fb_pbp(5931773)
#' }

ncaa_fb_pbp <- function(game_id) {
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
    tidyr::unnest(.data$possessions) %>%
    purrr::map_if(is.data.frame, list) %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(.data$plays, names_sep = "_") %>%
    dplyr::select(shortTitle, plays_teamId, time, plays_scoreText, plays_driveText, plays_visitingScore, plays_homeScore) %>%
    dplyr::rename("quarter" = .data$shortTitle,
           "team_id" = .data$plays_teamId) %>%
    dplyr::mutate(play_order = dplyr::row_number()) %>%
    janitor::clean_names()

  meta.data.df <- as.data.frame(pbp.json$meta$teams) %>% select(id, homeTeam, shortname)
  pbp.final <- merge(pbp, meta.data.df, by.x = "team_id", by.y = "id")
  pbp.final <- pbp.final %>%
    dplyr::arrange(play_order) %>%
    janitor::clean_names()

  return(pbp.final)
}
