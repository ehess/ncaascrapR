#' get_bb_pbp
#'
#' scrapes and parses pbp data for ncaa basketball games
#'
#' @param game_id the game_id for the specified game from stats.ncaa.com
#' @return a dataframe of cleaned, parsed play-by-play data for the specified `game_id`
get_bb_pbp <- function(game_id) {
  html <-
    paste0('https://stats.ncaa.org/game/play_by_play/', game_id) |>
    rvest::read_html()
  html |>
    rvest::html_nodes(xpath = '/html/body/div[2]') |>
    rvest::html_nodes('table') |>
    rvest::html_table() |> # pull pbp tables
    (\(x) { # filter out extraneous tables and add period for each table (helps deal w/ OT)
      table_counter <- 0
      table_list <- list()
      for(table in x){
        if (nrow(table)) {
          if(grepl('1\n\t      2',table$X2[1])){
            stored_period = table$X1[1] # store period
          }
          if (table$X1[1] == 'Time') {
            table_counter <- table_counter + 1
            table$period <- stored_period
            table_list[[table_counter]] <- table
          }
        }
      }
      return(table_list)
    })() |>
    dplyr::bind_rows() |> # combine all periods into single table
    tidyr::separate(X1, c('minute','second','centiseconds')) |>
    dplyr::mutate(
      across(c(minute, second, centiseconds),as.numeric),
      milliseconds = centiseconds * 10,
      time = lubridate::period(minute = minute, second = second) + lubridate::milliseconds(milliseconds)) |>
    dplyr::group_by(time,period) |>
    dplyr::summarize(
      away_text = paste0(X2, collapse = ';'),
      home_text = paste0(X4, collapse = ';'),
      score_events = paste0(X3, collapse = ';')
    ) |>
    tidyr::separate(period, c('period_first','period_second')) |>
    dplyr::arrange(period_second, period_first,desc(time)) |> # :sickos: fun sorting stuff here
    dplyr::ungroup() |>
    dplyr::mutate(
      period = paste(period_first,period_second),
      play_id = paste0(game_id, dplyr::row_number()),
      game_id = game_id,
      game_start_datetime = html |>
        rvest::html_node(xpath = '/html/body/div[2]/table[3]/tr[1]/td[2]') |>
        rvest::html_text(trim = T) |>
        lubridate::mdy_hm(tz = 'America/New_York'),
      game_start_date = game_start_datetime |>
        as.Date(tz = 'America/New_York'),
      game_start_time = game_start_datetime |>
        (\(x) paste0(
          lubridate::hour(x), ':', sprintf("%02d",lubridate::minute(x)), ' ET'
        ))(),
      home_team = home_text[score_events == 'Score'][1],
      away_team = away_text[score_events == 'Score'][1]) |>
    dplyr::filter(score_events != 'Score') |>
    dplyr::mutate(
      home_team_id = html |>
        rvest::html_node('body') |>
        rvest::html_node('div#contentarea') |>
        rvest::html_node('table.mytable') |>
        rvest::html_nodes('tr') |>
        rvest::html_nodes('a') |>
        rvest::html_attr('href') |>
        dplyr::nth(2) |>
        (\(x) gsub('/teams/', '', x))(),
      away_team_id = html |>
        rvest::html_node('body') |>
        rvest::html_node('div#contentarea') |>
        rvest::html_node('table.mytable') |>
        rvest::html_nodes('tr') |>
        rvest::html_nodes('a') |>
        rvest::html_attr('href') |>
        dplyr::nth(1) |>
        (\(x) gsub('/teams/', '', x))(),
      pos_team = dplyr::case_when(
        grepl(
          ' jumpball won| assist| 2pt| 3pt| rebound| freethrow| foulon| steal',
          away_text
        ) ~ away_team,
        grepl(
          ' jumpball won| assist| 2pt| 3pt| rebound| freethrow| foulon| steal',
          home_text
        ) ~ home_team,
        grepl(' turnover', away_text) ~ home_team,
        grepl(' turnover', home_text) ~ away_team,
        T ~ NA_character_
      ),
      pos_team = dplyr::case_when(is.na(pos_team) ~ dplyr::lag(pos_team),
                                  T ~ pos_team),
      pos_team_id = dplyr::case_when(
        pos_team == home_team ~ home_team_id,
        pos_team == away_team ~ away_team_id,
        T ~ NA_character_
      ),
      def_team = dplyr::case_when(
        pos_team == home_team ~ away_team,
        pos_team == away_team ~ home_team,
        T ~ NA_character_
      ),
      def_team_id = dplyr::case_when(
        pos_team_id == home_team_id ~ away_team_id,
        pos_team_id == away_team_id ~ home_team_id,
        T ~ NA_character_
      ),
      period_time_remaining = time,
      period_seconds_remaining = as.numeric(lubridate::seconds(period_time_remaining)),
      game_time_remaining = dplyr::case_when(
        period == '1st Half' ~ period_time_remaining + lubridate::ms('20:00'),
        T ~ period_time_remaining
      ),
      game_seconds_remaining = dplyr::case_when(
        period == '1st Half' ~ period_seconds_remaining + 1200,
        T ~ period_seconds_remaining
      ),
      pos_team_change = pos_team_id != dplyr::lag(pos_team_id),
      pos_team_change = dplyr::case_when(is.na(pos_team_change) ~ FALSE,
                                         T ~ pos_team_change),
      pos_number = cumsum(pos_team_change[!is.na(pos_team_change)]),
      desc = trimws(stringi::stri_replace_all_fixed(paste0(home_text, away_text),';',' ')),
      is_shot = grepl(
        '2pt|3pt|freethrow',
        desc
      ),
      field_goal_attempt = grepl(
        '2pt|3pt',
        desc
      ),
      two_point_attempt = grepl(
        '2pt',
        desc
      ),
      three_point_attempt = grepl(
        '3pt',
        desc
      ),
      jumpshot = grepl(
        'jumpshot|pullupjumpshot|floatingjumpshot|stepbackjumpshot',
        desc
      ),
      pullup_jumpshot = grepl(
        'pullupjumpshot',
        desc
      ),
      floating_jumpshot = grepl(
        'floatingjumpshot',
        desc
      ),
      stepback_jumpshot = grepl(
        'stepbackjumpshot',
        desc
      ),
      layup = grepl(
        'layup|drivinglayup',
        desc
      ),
      driving_layup = grepl(
        'drivinglayup',
        desc
      ),
      dunk = grepl(
        'dunk',
        desc
      ),
      hookshot = grepl(
        'hookshot',
        desc
      ),
      points_in_the_paint = grepl(
        'pointsinthepaint',
        desc
      ),
      from_turnover = grepl(
        'fromturnover',
        desc
      ),
      fast_break = grepl(
        'fastbreak',
        desc
      ),
      second_chance = grepl(
        '2ndchance',
        desc
      ),
      foul = grepl(
        'foul',
        desc
      ),
      assist = grepl(
        'assist',
        desc
      ),
      block = grepl(
        'block',
        desc
      ),
      free_throw = grepl(
        'freethrow',
        desc
      ),
      foul = grepl(
        'foul',
        desc
      ),
      personal_foul = grepl(
        'foul personal',
        desc
      ),
      offensive_foul = grepl(
        'foul offensive',
        desc
      ),
      technical_foul = grepl(
        'foul technical',
        desc
      ),
      technical_foul_class_a = grepl(
        'foul technical classa',
        desc
      ),
      technical_foul_class_b = grepl(
        'foul technical classb',
        desc
      ),
      flagrant_foul = grepl(
        'foul personal flagrant',
        desc
      ),
      flagrant_foul_one = grepl(
        'foul personal flagrant1',
        desc
      ),
      flagrant_foul_two = grepl(
        'foul personal flagrant2',
        desc
      ),
      away_score = stringi::stri_extract_all_regex(score_events, '[0-9]*-') |>
        lapply(stringi::stri_extract_all_regex, pattern = '[0-9]*', simplify = T) |>
        lapply(as.numeric) |>
        lapply(max,na.rm=T) |>
        unlist(),
      home_score = stringi::stri_extract_all_regex(score_events, '-[0-9]*') |>
        lapply(stringi::stri_extract_all_regex, pattern = '[0-9]*', simplify = T) |>
        lapply(as.numeric) |>
        lapply(max,na.rm=T) |>
        unlist(),
      away_score = dplyr::case_when(dplyr::row_number() == 1 ~ 0,
                                    is.infinite(away_score) & !is.na(dplyr::lag(away_score)) ~ dplyr::lag(away_score),
                                    is.infinite(away_score) & !is.na(dplyr::lead(away_score)) ~ dplyr::lead(away_score),
                                    T~away_score),
      home_score = dplyr::case_when(dplyr::row_number() == 1 ~ 0,
                                    is.infinite(home_score) & !is.na(dplyr::lag(home_score)) ~ dplyr::lag(home_score),
                                    is.infinite(home_score) & !is.na(dplyr::lead(home_score)) ~ dplyr::lead(home_score),
                                    T~home_score),

    )
}
