#' @title ncaa_fb_games
#' @description Obtains a list of games for a given week and year of the season
#' @param week the week number of the given season
#' @param year the year for the season you wish to look at
#' @return a data frame with all games for the given, week, year
#' @export
#' @examples \donttest{
#'   ncaa_fb_games(10, 2021)
#' }
ncaa_fb_games <- function(week, year) {
  if (!is.null(year) && !(is.numeric(year) && nchar(year) == 4)) {
    # Check if year is numeric, if not NULL
    cli::cli_abort("Enter valid year as a number (YYYY)")
  }
  if (!is.null(week) && !is.numeric(week) && nchar(week) > 2) {
    # Check if week is numeric, if not NULL
    cli::cli_abort("Enter valid week 1-15\n(14 for seasons pre-playoff, i.e. 2014 or earlier)")
  }

  base_url <- "https://data.ncaa.com/casablanca/scoreboard/football/fbs/"

  full_url <- paste(base_url, year, week, "scoreboard.json", sep="/")

  # Check for internet
  #check_internet()

  # Create the GET request and set response as res
  res <- httr::GET(full_url)

  # Check the result
  #check_status(res)

  games.json <- jsonlite::fromJSON(full_url, flatten = TRUE)

  games <- games.json$games

  #Return single value for column names
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
