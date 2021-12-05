#' ncaa_vb_games()
#' Pulls a list of volleyball games from the ncaa.com website for a given day
#'
#'
#' @param year Year, 4 digit format (YYYY)
#' @param month Month, 2 digit format (MM)
#' @param day Day, 2 digit format (DD)
#' @returns a data frame of games from the ncaa.com website

ncaa_vb_games <- function(year, month, day) {
  assertthat::assert_that(is.numeric(year) & nchar(year) == 4,
                          msg = 'Enter valid year as a number (YYYY)')
  assertthat::assert_that(is.numeric(month) & (month >= 1 & month <= 12),
                          msg = 'Enter month as a valid number 1-12')
  assertthat::assert_that(is.numeric(day) & (day >= 1 & day <= 31),
                          msg = 'Enter day as a valid number 1-31')

  base_url <- "https://data.ncaa.com/casablanca/scoreboard/volleyball-women/d1"

  month.string <- ifelse(nchar(month) == 1, paste0(0, month), month)
  day.string <- ifelse(nchar(day) == 1, paste0(0, day), day)

  full_url <- paste(base_url, year, month.string, day.string, "scoreboard.json", sep="/")
  res <- httr::GET(full_url)

  # Check the result
  #check_status(res)

  games.json <- jsonlite::fromJSON(full_url, flatten = TRUE)
  games <- games.json$games

  if (is_empty(games)) {
    print("No games found for given date")
    return()
  }

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
