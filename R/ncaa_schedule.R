#' @title ncaa_schedule
#'
#' @description Scrapes the schedule for a given team from the NCAA. Returns data similar to
#' `bigballR` but does so more efficiently and quickly.
#'
#' @param team_id the team ID for a given team
#' @return a data frame with the following columns: date, game_id, home_team, home_team_id,
#' away_team, away_team_id, home_score, away_score, is_neutral, details
#'
#' @examples
#' \donttest{
#' ncaa_schedule('501404') # womens soccer
#' ncaa_schedule('509092') # baseball
#' ncaa_schedule('509675') # mens tennis
#' }
#'
#' @export
ncaa_schedule <- function(team_id) {
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

  if(html_b |>
     rvest::html_text() != 'Unable to find player'){
    team_name <- html_b |>
      rvest::html_node('body') |>
      rvest::html_node('#contentarea') |>
      rvest::html_node('#stats_player_person_id') |>
      rvest::html_node('option') |>
      rvest::html_text()
    game_id_table <- data.frame(
      Date = html_b |>
        rvest::html_node('body') |>
        rvest::html_node('div#contentarea') |>
        rvest::html_node('div#game_breakdown_div') |>
        rvest::html_nodes(xpath='table/tr/td/table/tr/td[count(table/tr/td/table/tr/th[.="Result"]/preceding-sibling::th)+1]') |>
        rvest::html_text(trim=T) |>
        unlist() |>
        (\(x) x[x!=''])() |>
        (\(x) x[-c(1)])(),
      Opponent_Clean = html_b |>
        rvest::html_node('body') |>
        rvest::html_node('div#contentarea') |>
        rvest::html_node('div#game_breakdown_div') |>
        rvest::html_nodes(xpath='table/tr/td/table/tr/td[count(table/tr/td/table/tr/th[.="Result"]/preceding-sibling::th)+2]') |>
        lapply(\(x) {
          if(length(rvest::html_attr(x,'class'))){
            if(!is.na(rvest::html_attr(x,'class'))){
              return(rvest::html_text(x,trim=T))
            }
          }}) |>
        unlist() |>
        stringi::stri_replace_all(regex = '((?=(.*)) @ (.*)$)|(@ )', ''),
      Result = html_b |>
        rvest::html_node('body') |>
        rvest::html_node('div#contentarea') |>
        rvest::html_node('div#game_breakdown_div') |>
        rvest::html_nodes(xpath='table/tr/td/table/tr/td[count(table/tr/td/table/tr/th[.="Result"]/preceding-sibling::th)+3]') |>
        rvest::html_text(trim=T) |>
        unlist() |>
        (\(x) x[x!=''])() |>
        stringi::stri_replace_all(regex = ' -', '-') |>
        stringi::stri_replace_all(regex = '- ', '-') |>
        stringi::stri_replace_all(regex = '(\\(([0-9]* OT)\\))|(\\(([0-9]*OT)\\))', '') |>
        trimws(),
      game_id = html_b |>
        rvest::html_node('body') |>
        rvest::html_node('div#contentarea') |>
        rvest::html_node('div#game_breakdown_div') |>
        rvest::html_nodes(xpath='table/tr/td/table/tr/td[count(table/tr/td/table/tr/th[.="Result"]/preceding-sibling::th)+3]') |>
        sapply(\(x) {
          id <- rvest::html_nodes(x, 'a') |>
            rvest::html_attr('href')
          if(!length(id)){
            id <- NA_character_
          }
          text <- x |>
            rvest::html_text(trim=T)
          if(nchar(text)){
            return(id)
          }
        },simplify=T) |>
        unlist() |>
        stringi::stri_replace_all(regex = '(/game/index/)|(\\?org_id=[0-9]*$)', '')
    )
  } else { # this is a hilariously hacky way to grab team short names
    team_name <- html_a |>
      rvest::html_nodes(xpath='/html/body/div[2]/a[2]') |>
      rvest::html_attr('href') |>
      (\(x) glue::glue('https://stats.ncaa.org/{x}'))() |>
      rvest::read_html() |>
      rvest::html_node(xpath='/html/body/div[2]/div[2]/span[2]') |>
      rvest::html_text() |>
      (\(x) gsub('(We do not have stats for )|(\\.  Please select an institution from the list below\\.)','',x))()
    game_id_table <- data.frame(Date = NA,
                                Opponent_Clean = NA,
                                Result = NA,
                                game_id = NA)
  }

  year_format <- c("2013-14", "2014-15", "2015-16", "2016-17", "2017-18")

  team_ids <- data.frame(id = html_a |> rvest::html_nodes(xpath = '//*[@id="year_list"]/option') |>
                           rvest::html_attr("value"),
                         year = html_a |> rvest::html_nodes(xpath = '//*[@id="year_list"]/option') |>
                           rvest::html_text())

  selected_year <- team_ids |>
    dplyr::filter(id == team_id) |>
    dplyr::select(year) |>
    dplyr::first()

  opponent_id_table <- data.frame(Opponent_Clean = html_a |>
                                    rvest::html_node('body') |>
                                    rvest::html_node('#contentarea') |>
                                    rvest::html_node('table') |>
                                    rvest::html_node('tr') |>
                                    rvest::html_node('td') |>
                                    rvest::html_nodes('a') |>
                                    rvest::html_text() |>
                                    (\(x) {
                                      x[!stringi::stri_detect(x, regex = '^[W|L|T] ([0-9]*)\\s*-\\s*([0-9]*)')]
                                    })() |>
                                    stringi::stri_replace_all(regex = '^#([0-9]*)', '') |>
                                    stringi::stri_replace_all(regex = '@\\s', '') |>
                                    trimws(),
                                  opponent_id = if(selected_year %in% year_format) { html_a |>
                                      rvest::html_node('body') |>
                                      rvest::html_node('#contentarea') |>
                                      rvest::html_node('table') |>
                                      rvest::html_nodes('tr') |>
                                      rvest::html_nodes('a') |>
                                      rvest::html_attr('href') |>
                                      (\(x) {
                                        x[stringi::stri_detect(x, regex = 'team/[0-9]*/[0-9]*$')]
                                      })() |>
                                      (\(x) { glue::glue('https://stats.ncaa.org{x}') })() |>
                                      sapply(\(y) {
                                        httr::GET(y, httr::add_headers("user-agent" = "Mozilla/5.0")) |>
                                          (\(z) {
                                            unlist(lapply(z$all_headers, function(x) {
                                              x$headers$location
                                            }))
                                          })()
                                      }) |>
                                      stringi::stri_replace_all(regex = 'https://stats.ncaa.org/teams/', '')
                                  } else {

                                    html_a |>
                                      rvest::html_node('body') |>
                                      rvest::html_node('#contentarea') |>
                                      rvest::html_node('table') |>
                                      rvest::html_nodes('tr') |>
                                      rvest::html_nodes('a') |>
                                      rvest::html_attr('href') |>
                                      (\(x) {
                                        x[stringi::stri_detect(x, regex = 'teams/[0-9]*$')]
                                      })() |>
                                      stringi::stri_replace_all(regex = '/teams/', '')
                                  }
  )

  html_a |>
    xml2::xml_find_all(".//br") |>
    xml2::xml_add_sibling("p", "x_break")

  html_a |>
    rvest::html_node('body') |>
    rvest::html_node('#contentarea') |>
    rvest::html_node('table') |>
    rvest::html_node('tr') |>
    rvest::html_node('td') |>
    rvest::html_table() |>
    plyr::rename(replace = c("X1" = "Date",
                             "X2" = "Opponent",
                             "X3" = "Result",
                             "X4" = "Attendance"),
                 warn_missing = FALSE) |>
    dplyr::filter(Date != '' & Date != "Schedule/Results" & Date != "Date") |>
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
      Opponent_Clean = trimws(gsub('@ |^\\#[0-9]* |^[0-9]* | x_break|^\\@#[0-9]* ','',stringi::stri_extract_all_regex(paste0(Opponent,' x_break'),'^.+?(?=x_break)')))) |>
    dplyr::left_join(opponent_id_table, by=c('Opponent_Clean')) |>
    dplyr::mutate(
      event = trimws(
        stringi::stri_replace_all(
          stringi::stri_replace_all_fixed(Opponent, Opponent_Clean, ''),
          regex = '#([0-9]*)',
          ''
        )
      ),
      event = dplyr::case_when(
        stringi::stri_detect(event, regex = '\\((.*)\\)') ~ gsub(
          '\\(|\\)',
          '',
          stringi::stri_extract_all(event, regex = '\\((.*)\\)')
        ),
        stringi::stri_detect(event, regex = '^@(.*)') ~ gsub('^@(.*)', '', event),
        T ~ event
      ),
      event = gsub('x_break','',event),
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
          home_team == team_name ~ stringi::stri_extract(Result, regex = '[0-9]*\\s*-'),
          T ~ stringi::stri_extract(Result, regex = '-\\s*[0-9]*')
        )
      )),
      away_score = as.integer(gsub(
        '-',
        '',
        dplyr::case_when(
          away_team == team_name ~ stringi::stri_extract(Result, regex = '[0-9]*\\s*-'),
          T ~ stringi::stri_extract(Result, regex = '-\\s*[0-9]*')
        )
      )),
      Attendance = dplyr::across(.cols = any_of("Attendance"), ~ suppressWarnings(as.integer(gsub(
        ',', '', Attendance
      )))),
      Result = trimws(
        stringi::stri_replace_all(Result, regex = '(\\(([0-9]* OT)\\))|(\\(([0-9]*OT)\\))', '')
      )
    ) |>
    dplyr::left_join(game_id_table, by = c('Date', 'Opponent_Clean', 'Result')) |>
    dplyr::distinct(game_id,Date,opponent_id, .keep_all=T) |>
    dplyr::mutate(event = dplyr::case_when(event == '' ~ NA_character_,
                                           T~ event)) |>
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
      attendance = Attendance
    )
}
