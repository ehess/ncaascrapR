#' @title get_team_schedule
#'
#' @description Scrapes the schedule for a given team from the NCAA. Returns data similar to
#' `bigballR` but does so more efficiently and quickly.
#'
#' @param team_id the team ID for a given team
#' @return a data frame with the following columns: date, game_id, home_team, home_team_id,
#' away_team, away_team_id, home_score, away_score, is_neutral, details
#' @export
get_team_schedule <- function(team_id) {
  html_a <- glue::glue('https://stats.ncaa.org/teams/{team_id}') |>
    rvest::read_html()
  game_by_game_url <- html_a |>
    rvest::html_node('#contentarea') |>
    rvest::html_nodes('a') |>
    rvest::html_attr('href') |>
    (\(x) {
      x[stringi::stri_detect(x, regex = 'player/game_by_game')]
    })() |>
    dplyr::first()
  html_b <-
    glue::glue('https://stats.ncaa.org/{game_by_game_url}') |>
    rvest::read_html()
  team_name <- html_b |>
    rvest::html_node('body') |>
    rvest::html_node('#contentarea') |>
    rvest::html_node('#stats_player_person_id') |>
    rvest::html_node('option') |>
    rvest::html_text()
  game_id_table <- html_b |>
    rvest::html_node('body') |>
    rvest::html_node('div#contentarea') |>
    rvest::html_node('div#game_breakdown_div') |>
    rvest::html_node('table') |>
    rvest::html_node('tr') |>
    rvest::html_node('td') |>
    rvest::html_table() |>
    dplyr::select(X1, X2, X3) |>
    dplyr::filter(X1 != '' & X3 != '-') |>
    dplyr::slice(-c(1:2)) |>
    dplyr::group_by(X1) |>
    dplyr::mutate(
      game_num = dplyr::row_number(),
      X1 = dplyr::case_when(max(game_num) > 1 ~ paste0(X1, '(', game_num, ')'),
                            T ~ X1)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      game_id = html_b |>
        rvest::html_node('body') |>
        rvest::html_node('div#contentarea') |>
        rvest::html_node('div#game_breakdown_div') |>
        rvest::html_nodes('a') |>
        rvest::html_attr('href') |>
        (\(x) {
          x[stringi::stri_detect(x, regex = '^/game/index/[0-9]*\\?org_id=[0-9]*$')]
        })() |>
        stringi::stri_replace_all(regex = '(/game/index/)|(\\?org_id=[0-9]*$)', ''),
      X2 = stringi::stri_replace_all(X2, regex = '((?=(.*)) @ (.*)$)|(@ )', ''),
      X3 = stringi::stri_replace_all(X3, regex = ' -', '-'),
      X3 = stringi::stri_replace_all(X3, regex = '- ', '-'),
      X3 = stringi::stri_replace_all(X3, regex = '(\\(([0-9]* OT)\\))|(\\(([0-9]*OT)\\))', '')
    ) |>
    dplyr::select(
      Date = X1,
      Opponent_Clean = X2,
      Result = X3,
      game_id
    )
  html_a |>
    rvest::html_node('body') |>
    rvest::html_node('#contentarea') |>
    rvest::html_node('table') |>
    rvest::html_node('tr') |>
    rvest::html_node('td') |>
    rvest::html_table() |>
    dplyr::filter(Date != '') |>
    dplyr::mutate(
      is_neutral = stringi::stri_detect(Opponent, regex = '(.+)\\@'),
      loc = dplyr::case_when(is_neutral ~ trimws(gsub(
        ' \\((.*)\\)',
        '',
        gsub(
          '(.+)\\@',
          '',
          stringi::stri_extract(Opponent, regex = '(.+)\\@(.*)')
        )
      )),
      T ~ NA_character_),
      opponent_id = html_a |>
        rvest::html_node('body') |>
        rvest::html_node('#contentarea') |>
        rvest::html_node('table') |>
        rvest::html_nodes('tr') |>
        rvest::html_nodes('a') |>
        rvest::html_attr('href') |>
        (\(x) {
          x[stringi::stri_detect(x, regex = 'teams/[0-9]*$')]
        })() |>
        stringi::stri_replace_all(regex = '/teams/', ''),
      Opponent_Clean = html_a |>
        rvest::html_node('body') |>
        rvest::html_node('#contentarea') |>
        rvest::html_node('table') |>
        rvest::html_node('tr') |>
        rvest::html_node('td') |>
        rvest::html_nodes('a') |>
        rvest::html_text() |>
        (\(x) {
          x[!stringi::stri_detect(x, regex = '^[W|L|T] ([0-9]*)-([0-9]*)')]
        })() |>
        stringi::stri_replace_all(regex = '^#([0-9]*)', '') |>
        trimws(),
      event = trimws(
        stringi::stri_replace_all(
          stringi::stri_replace_all_fixed(Opponent, Opponent_Clean, ''),
          regex = '#([0-9]*)',
          ''
        )
      ),
      event = dplyr::case_when(stringi::stri_detect(event,regex='\\((.*)\\)') ~ gsub('\\(|\\)','',stringi::stri_extract_all(event,regex='\\((.*)\\)')),
                               stringi::stri_detect(event,regex='^@(.*)') ~ gsub('^@(.*)','',event),
                               T ~ event),
      details = dplyr::case_when(
        stringi::stri_detect(Result, regex = '^(W|L|T) ([0-9]*)-([0-9]*)$') ~ NA_character_,
        stringi::stri_detect(Result, regex = '^(W|L|T) ([0-9]*)-([0-9]*)') ~ trimws(gsub(
          '(^(W|L|T) ([0-9]*)-([0-9]*))|\\(|\\)', '', Result
        )),
        T ~ Result
      ),
      away_team = trimws(
        dplyr::case_when(
          stringi::stri_detect(Opponent, regex = '^@') ~ team_name,
          T ~ Opponent_Clean
        )
      ),
      home_team = trimws(
        dplyr::case_when(
          !stringi::stri_detect(Opponent, regex = '^@') ~ team_name,
          T ~ Opponent_Clean
        )
      ),
      away_team_id = dplyr::case_when(
        stringi::stri_detect(Opponent, regex = '^@') ~ as.character(team_id),
        T ~ as.character(opponent_id)
      ),
      home_team_id = dplyr::case_when(
        stringi::stri_detect(Opponent, regex = '^@') ~ as.character(opponent_id),
        T ~ as.character(team_id)
      ),
      home_score = as.integer(gsub(
        '-',
        '',
        dplyr::case_when(
          home_team == team_name ~ stringi::stri_extract(Result, regex = '[0-9]*-'),
          T ~ stringi::stri_extract(Result, regex = '-[0-9]*')
        )
      )),
      away_score = as.integer(gsub(
        '-',
        '',
        dplyr::case_when(
          away_team == team_name ~ stringi::stri_extract(Result, regex = '[0-9]*-'),
          T ~ stringi::stri_extract(Result, regex = '-[0-9]*')
        )
      )),
      Attendance = suppressWarnings(as.integer(gsub(',','',Attendance))),
      Result = trimws(stringi::stri_replace_all(Result, regex = '(\\(([0-9]* OT)\\))|(\\(([0-9]*OT)\\))', ''))
    ) |>
    dplyr::left_join(game_id_table, by = c('Date', 'Opponent_Clean', 'Result')) |>
    dplyr::select(
      date = Date,
      game_id,
      home_team,
      home_team_id,
      away_team,
      away_team_id,
      home_score,
      away_score,
      is_neutral,
      event,
      details,
      attendance = Attendance
    )
}
