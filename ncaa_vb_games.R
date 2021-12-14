
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
    html_table() %>%
    as.data.frame() %>%
    filter(Date != "")


  sched$opponent_slug <- sched_html %>%
    rvest::html_elements("td:nth-child(2) > a" ) %>%
    rvest::html_attr("href")

  sched.played <- sched %>%
    dplyr::filter(!(.data$Result %in% c("Canceled","Ppd", "")))

  sched.played$slug <- sched_html %>%
    rvest::html_elements("td .skipMask") %>%
    rvest::html_attr("href")

  schedule <- sched.played %>%
    dplyr::mutate(.data$Home = !grepl('@', .data$Opponent)) %>%
    tidyr::separate(.data$opponent_slug, c("Teams", "Team_Id"), sep = "/teams/") %>%
    tidyr::separate(.data$slug, c("Empty", "Contests", "Game_Id", "box_score"), sep = "/") %>%
    tidyr::separate(.data$Result, c("Result", "Team_score", "Opponent_score")) %>%
    dplyr::select(Date, Opponent, Result, Team_score, Opponent_score, Attendance, Team_Id, Game_Id, Home) %>%
    janitor::clean_names()

  return(schedule)
}
