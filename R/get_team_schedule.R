#' @title get_team_schedule
#'
#' @description Scrapes the schedule for a given team from the NCAA. Returns data similar to
#' `bigballR` but does so more efficiently and quickly.
#'
#' @param team_id the team ID for a given team
#' @param team_url portion of team URL after /teams/
#' @return a data frame with the following columns: date, game_id, home_team, home_team_id,
#' away_team, away_team_id, home_score, away_score, is_neutral, details
#' @export
get_team_schedule <-
  function(team_id = NA_character_, team_url = NA_character_) {
    if (is.na(team_id) & is.na(team_url)) {
      cli::cli_abort("Please specify one of `team_url` or `team_id`.")
    } else if (!is.na(team_id) & !is.na(team_url)) {
      cli::cli_abort("Please specify only one of `team_url` or `team_id`.")
    } else if (!is.na(team_id)) {
      url <- glue::glue('https://stats.ncaa.org/teams/{team_id}')
      url = url(url, "rb")
      html_a <-  url |>
        rvest::read_html()
      close(url)
    } else {
      url <- glue::glue('https://stats.ncaa.org/team/{team_url}')
      url = url(url, "rb")
      html_a <-  url |>
        rvest::read_html()
      close(url)
    }
    team_id <- html_a |>
      rvest::html_node(xpath = '//*[@id="sport_list"]') |>
      rvest::html_nodes("option") |>
      rvest::html_attr("value") |>
      dplyr::first()
    game_by_game_url <- html_a |>
      rvest::html_node('#contentarea') |>
      rvest::html_nodes('a') |>
      rvest::html_attr('href') |>
      (\(x) {
        x[stringi::stri_detect(x, regex = 'player/game_by_game')]
      })() |>
      dplyr::first()
    url <- glue::glue('https://stats.ncaa.org/{game_by_game_url}')
    url = url(url, "rb")
    html_b <- url |>
      rvest::read_html()
    close(url)
    team_name <- html_b |>
      rvest::html_node('body') |>
      rvest::html_node('#contentarea') |>
      rvest::html_node('#stats_player_person_id') |>
      rvest::html_node('option') |>
      rvest::html_text()
    id_table <- data.frame(
      name = html_a |>
        rvest::html_node('body') |>
        rvest::html_node('#contentarea') |>
        rvest::html_node('table') |>
        rvest::html_nodes(xpath = '//table/tbody/tr/td[count(//table/thead/tr/th[.="$Opponent"]/preceding-sibling::th)+2]') |>
        rvest::html_nodes('a') |>
        rvest::html_text() |>
        trimws(),
      id = html_a |>
        rvest::html_node('body') |>
        rvest::html_node('#contentarea') |>
        rvest::html_node('table') |>
        rvest::html_nodes(xpath = '//table/tbody/tr/td[count(//table/thead/tr/th[.="$Opponent"]/preceding-sibling::th)+2]') |>
        rvest::html_nodes('a') |>
        rvest::html_attr('href') |>
        trimws() |>
        stringi::stri_replace_all(regex = "/teams/", replacement = '')
    ) |>
      dplyr::bind_rows(data.frame(name = team_name,
                                  id = team_id))
    game_id_table <- data.frame(game_id = html_b |>
                                  rvest::html_node(xpath='//*[@id="game_breakdown_div"]') |>
                                  rvest::html_nodes('table') |>
                                  rvest::html_nodes(xpath = 'tr/td[count(tr/th[.="$Result"]/preceding-sibling::th)+3]') |>
                                  rvest::html_nodes("a") |>
                                  rvest::html_attr("href") |>
                                  (\(x) {
                                    x[stringi::stri_detect(x, regex = '^/game/index/[0-9]*\\?org_id=[0-9]*$')]
                                  })() |>
                                  stringi::stri_replace_all(regex = '(/game/index/)|(\\?org_id=[0-9]*$)', ''),
                                result = html_b |>
                                  rvest::html_node(xpath='//*[@id="game_breakdown_div"]') |>
                                  rvest::html_nodes('table') |>
                                  rvest::html_nodes(xpath = 'tr/td[count(tr/th[.="$Result"]/preceding-sibling::th)+3]') |>
                                  rvest::html_nodes("a") |>
                                  rvest::html_attr("href"))
    html_b |>
      rvest::html_node('body') |>
      rvest::html_node('div#contentarea') |>
      rvest::html_node('div#game_breakdown_div') |>
      rvest::html_node('table') |>
      rvest::html_node('tr') |>
      rvest::html_node('td') |>
      rvest::html_table() |>
      dplyr::select(X1, X2, X3) |>
      dplyr::filter(X1 != '' & X3 != '-' & !stringi::stri_detect_regex(X3,'^[0-9]')) |>
      dplyr::slice(-c(1:2)) |>
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
        is_neutral = stringi::stri_detect(X2, regex = '(.+)\\@'),
        Opponent = stringi::stri_replace_all(X2, regex = '((?=(.*)) @ (.*)$)|(@ )', ''),
        away_team = trimws(
          dplyr::case_when(stringi::stri_detect(X2, regex = '^@') ~ team_name,
                           T ~ Opponent)
        ),
        home_team = trimws(
          dplyr::case_when(
            !stringi::stri_detect(X2, regex = '^@') ~ team_name,
            T ~ Opponent
          )
        ),
        away_team_id = plyr::mapvalues(away_team, from = id_table$name, to = id_table$id, warn_missing = F),
        home_team_id = plyr::mapvalues(home_team, from = id_table$name, to = id_table$id, warn_missing = F),
        X3 = stringi::stri_replace_all(X3, regex = ' -', '-'),
        X3 = stringi::stri_replace_all(X3, regex = '- ', '-'),
        X3 = trimws(
          stringi::stri_replace_all(X3, regex = '(\\(([0-9]* OT)\\))|(\\(([0-9]*OT)\\))', '')
        ),
        home_score = as.integer(gsub(
          '-',
          '',
          dplyr::case_when(
            home_team == team_name ~ stringi::stri_extract(X3, regex = '[0-9]*-'),
            T ~ stringi::stri_extract(X3, regex = '-[0-9]*')
          )
        )),
        away_score = as.integer(gsub(
          '-',
          '',
          dplyr::case_when(
            away_team == team_name ~ stringi::stri_extract(X3, regex = '[0-9]*-'),
            T ~ stringi::stri_extract(X3, regex = '-[0-9]*')
          )
        ))
      ) |>
      dplyr::select(
        Date = X1,
        game_id,
        is_neutral,
        away_team,
        home_team,
        away_team_id,
        home_team_id,
        away_score,
        home_score
      ) # |>
      # dplyr::bind_rows(
      #   html_b |>
      #     rvest::html_node('body') |>
      #     rvest::html_node('div#contentarea') |>
      #     rvest::html_node('div#game_breakdown_div') |>
      #     rvest::html_node('table') |>
      #     rvest::html_node('tr') |>
      #     rvest::html_node('td') |>
      #     rvest::html_table() |>
      #     dplyr::select(X1, X2, X3) |>
      #     dplyr::filter(X3 == '-' | stringi::stri_detect_regex(X3,'^[0-9]')) |>
      #     dplyr::mutate(
      #       game_id = NA_character_,
      #       is_neutral = stringi::stri_detect(X2, regex = '(.+)\\@'),
      #       Opponent = stringi::stri_replace_all(X2, regex = '((?=(.*)) @ (.*)$)|(@ )', ''),
      #       away_team = trimws(
      #         dplyr::case_when(stringi::stri_detect(X2, regex = '^@') ~ team_name,
      #                          T ~ Opponent)
      #       ),
      #       home_team = trimws(
      #         dplyr::case_when(
      #           !stringi::stri_detect(X2, regex = '^@') ~ team_name,
      #           T ~ Opponent
      #         )
      #       ),
      #       away_team_id = plyr::mapvalues(away_team, from = id_table$name, to = id_table$id, warn_missing = F),
      #       home_team_id = plyr::mapvalues(home_team, from = id_table$name, to = id_table$id, warn_missing = F),
      #       X3 = stringi::stri_replace_all(X3, regex = ' -', '-'),
      #       X3 = stringi::stri_replace_all(X3, regex = '- ', '-'),
      #       X3 = trimws(
      #         stringi::stri_replace_all(X3, regex = '(\\(([0-9]* OT)\\))|(\\(([0-9]*OT)\\))', '')
      #       ),
      #       home_score = as.integer(gsub(
      #         '-',
      #         '',
      #         dplyr::case_when(
      #           home_team == team_name ~ stringi::stri_extract(X3, regex = '[0-9]*-'),
      #           T ~ stringi::stri_extract(X3, regex = '-[0-9]*')
      #         )
      #       )),
      #       away_score = as.integer(gsub(
      #         '-',
      #         '',
      #         dplyr::case_when(
      #           away_team == team_name ~ stringi::stri_extract(X3, regex = '[0-9]*-'),
      #           T ~ stringi::stri_extract(X3, regex = '-[0-9]*')
      #         )
      #       ))
      #     ) |>
      #     dplyr::select(
      #       Date = X1,
      #       game_id,
      #       is_neutral,
      #       away_team,
      #       home_team,
      #       away_team_id,
      #       home_team_id,
      #       away_score,
      #       home_score
      #     )
      # )
  }
