#' @title ncaa_vb_games
#' @description Pulls a list of volleyball games from the NCAA website for a given day
#'
#' @param team_id the id for the team for that season
#' @returns a data frame of games from the NCAA website
#' @import rvest
#' @importFrom cli cli_abort
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom dplyr select
#' @importFrom tidyr separate
#' @export
#'
ncaa_vb_games <- function(team_id) {
  base_url <- "https://stats.ncaa.org/teams"

  full_url <- paste(base_url, team_id, sep="/")

  base <- rvest::read_html(full_url)

  sched_html <- base %>%
    rvest::html_elements("fieldset") %>%
    rvest::html_elements("table")

  #Class for table isn't consistent across years
  #Using table position which seems consistent
  #filter out empty divider rows
  sched <- sched_html %>%
    rvest::html_table() %>%
    as.data.frame() %>%
    dplyr::filter(Date != "")


  sched$opponent_slug <- sched_html %>%
    rvest::html_elements("td:nth-child(2) > a" ) %>%
    rvest::html_attr("href")

  sched.played <- sched %>%
    dplyr::filter(!(.data$Result %in% c("Canceled","Ppd", "")))

  sched.played$slug <- sched_html %>%
    rvest::html_elements("td .skipMask") %>%
    rvest::html_attr("href")

  schedule <- sched.played %>%
    dplyr::mutate(Home = !grepl('@', .data$Opponent)) %>%
    tidyr::separate(.data$opponent_slug, c("Teams", "Team_Id"), sep = "/teams/") %>%
    tidyr::separate(.data$slug, c("Empty", "Contests", "Game_Id", "box_score"), sep = "/") %>%
    tidyr::separate(.data$Result, c("Result", "Team_score", "Opponent_score")) %>%
    dplyr::select(Date, Opponent, Result, Team_score, Opponent_score, Attendance, Team_Id, Game_Id, Home) %>%
    janitor::clean_names()

  return(games.final)
}
