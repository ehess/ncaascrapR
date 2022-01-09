game_id <- '561578'
if (!exists('html')) {
  html <-
    paste0('https://stats.ncaa.org/game/play_by_play/', game_id) |>
    rvest::read_html()
  box_html <-
    paste0('https://stats.ncaa.org/game/box_score/', game_id) |>
    rvest::read_html()
}

rosters <- box_html |>
  rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
  rvest::html_table() |>
  janitor::row_to_names(2) |>
  dplyr::filter(Player != 'Totals') |>
  dplyr::mutate(
    player = gsub(',', '', sub("(^.*),\\s(.*$)", "\\2 \\1", Player)),
    player_id = box_html |>
      rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
      rvest::html_nodes('a') |>
      rvest::html_attr('href') |>
      (\(x) gsub(
        '_seq=',
        '',
        stringi::stri_extract_all_regex(x, '_seq=([0-9]*)$')
      ))() |>
      (\(x) c(x, ''))(),
    team = box_html |>
      rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
      rvest::html_node('.heading') |>
      rvest::html_text() |>
      trimws(),
    team_id = box_html |>
      rvest::html_nodes(xpath = '/html/body/div[2]/table[1]/tr/td/a') |>
      rvest::html_attr('href') |>
      (\(x) gsub('/teams/', '', x))() |>
      dplyr::nth(1),
    pos = Pos
  ) |>
  dplyr::select(player:pos) |>
  dplyr::bind_rows(
    box_html |>
      rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
      rvest::html_table() |>
      janitor::row_to_names(2) |>
      dplyr::filter(Player != 'Totals') |>
      dplyr::mutate(
        player = gsub(',', '', sub("(^.*),\\s(.*$)", "\\2 \\1", Player)),
        player_id = box_html |>
          rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
          rvest::html_nodes('a') |>
          rvest::html_attr('href') |>
          (\(x) gsub(
            '_seq=',
            '',
            stringi::stri_extract_all_regex(x, '_seq=([0-9]*)$')
          ))()  |>
          (\(x) c(x, ''))(),
        team = box_html |>
          rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
          rvest::html_node('.heading') |>
          rvest::html_text() |>
          trimws(),
        team_id = box_html |>
          rvest::html_nodes(xpath = '/html/body/div[2]/table[1]/tr/td/a') |>
          rvest::html_attr('href') |>
          (\(x) gsub('/teams/', '', x))() |>
          dplyr::nth(2),
        pos = Pos
      ) |>
      dplyr::select(player:pos)
  )

html |>
  rvest::html_nodes(xpath = '/html/body/div[2]') |>
  rvest::html_nodes('table') |>
  rvest::html_table() |> # pull pbp tables
  (\(x) {
    # filter out extraneous tables and add period for each table (helps deal w/ OT)
    table_counter <- 0
    table_list <- list()
    for (table in x) {
      if (nrow(table) & 'X2' %in% colnames(table)) {
        if (grepl('1\n\t      2', table$X2[1])) {
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
  tidyr::separate(X1, c('minute', 'second', 'centiseconds')) |>
  dplyr::mutate(
    across(c(minute, second, centiseconds), as.numeric),
    centiseconds = dplyr::case_when(is.na(centiseconds) ~ 0,
                                    T ~ centiseconds),
    milliseconds = centiseconds * 10,
    time = lubridate::period(minute = minute, second = second) + lubridate::milliseconds(milliseconds),
    time = dplyr::case_when(X3 == 'Score' ~ lubridate::period(minute = 20, second = 0),
                            T ~ time)
  ) |>
  tidyr::separate(period, c('period_first', 'period_second')) |>
  dplyr::arrange(period_second, period_first, desc(time)) |> # :sickos: fun sorting stuff here
  dplyr::ungroup() |>
  dplyr::mutate(
    pbp_version = dplyr::case_when(X3 == 'game start' ~ 'V2',
                                   T ~ 'V1'),
    period = paste(period_first, period_second),
    play_id = paste0(game_id, dplyr::row_number()),
    game_id = game_id,
    game_start_datetime = html |>
      rvest::html_node(xpath = '/html/body/div[2]/table[3]/tr[1]/td[2]') |>
      rvest::html_text(trim = T) |>
      lubridate::mdy_hm(tz = 'America/New_York'),
    game_start_datetime = dplyr::case_when(
      is.na(game_start_datetime) ~ html |>
        rvest::html_node(xpath = '/html/body/div[2]/table[3]/tr[1]/td[2]') |>
        rvest::html_text(trim = T) |>
        lubridate::mdy(tz = 'America/New_York'),
      T ~ game_start_datetime
    ),
    game_start_date = game_start_datetime |>
      as.Date(tz = 'America/New_York'),
    game_start_time = game_start_datetime |>
      (\(x) paste0(
        lubridate::hour(x),
        ':',
        sprintf("%02d", lubridate::minute(x)),
        ' ET'
      ))(),
    game_start_time = dplyr::case_when(
      is.na(
        html |>
          rvest::html_node(xpath = '/html/body/div[2]/table[3]/tr[1]/td[2]') |>
          rvest::html_text(trim = T) |>
          lubridate::mdy_hm(tz = 'America/New_York')
      ) ~ NA_character_,
      T ~ game_start_time
    ),
    away_team = html |>
      rvest::html_nodes(xpath = '/html/body/div[2]/table[6]/tr[1]/td[2]') |>
      rvest::html_text(),
    home_team = html |>
      rvest::html_nodes(xpath = '/html/body/div[2]/table[6]/tr[1]/td[4]') |>
      rvest::html_text(),
    away_team_id = html |>
      rvest::html_node('body') |>
      rvest::html_node('div#contentarea') |>
      rvest::html_node('table.mytable') |>
      rvest::html_nodes('tr') |>
      rvest::html_nodes('a') |>
      rvest::html_attr('href') |>
      dplyr::nth(2) |>
      (\(x) gsub('/teams/', '', x))(),
    home_team_id = html |>
      rvest::html_node('body') |>
      rvest::html_node('div#contentarea') |>
      rvest::html_node('table.mytable') |>
      rvest::html_nodes('tr') |>
      rvest::html_nodes('a') |>
      rvest::html_attr('href') |>
      dplyr::nth(1) |>
      (\(x) gsub('/teams/', '', x))(),
    pos_team = dplyr::case_when(
      pbp_version == 'V2' ~ dplyr::case_when(
        grepl(
          ' jumpball won| assist| 2pt| 3pt| rebound| freethrow| foulon| steal',
          X2
        ) ~ away_team,
        grepl(
          ' jumpball won| assist| 2pt| 3pt| rebound| freethrow| foulon| steal',
          X4
        ) ~ home_team,
        grepl(' jumpball lost| turnover', X2) ~ home_team,
        grepl(' jumpball lost| turnover', X4) ~ away_team,
        T ~ NA_character_
      ),
      T ~ dplyr::case_when(
        grepl(' Rebound| made| missed| Assist| Steal',
              X2) ~ away_team,
        grepl(' Rebound| made| missed| Assist| Steal', X4) ~ home_team,
        grepl(' Foul| Turnover', X2) ~ home_team,
        grepl(' Foul| Turnover', X4) ~ away_team,
        T ~ NA_character_
      )
    ),
    pos_team = dplyr::case_when(dplyr::row_number() == 1 & pbp_version == 'V1' & grepl(dplyr::lead(X2),' Turnover') ~ home_team, # fixing a very annoying edge case where a team wins jump ball
                                dplyr::row_number() == 1 & pbp_version == 'V1' & grepl(dplyr::lead(X4),' Turnover') ~ away_team, # then immediately loses it on a turnover
                                T ~ pos_team),
    pos_team = dplyr::case_when(pbp_version == 'V1' & X3 == 'Score' & period == '2nd Half' ~ dplyr::case_when(pos_team[1] == home_team ~ away_team,
                                                                                                              T ~ home_team),
                                T ~ pos_team)
  ) |>
  tidyr::fill(pos_team, .direction = "down") |>
  tidyr::fill(pos_team, .direction = "up") |>
  dplyr::mutate(
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
    # putting these columns on temporary hold, V1 PBP completely breaks them
    # pos_team_change = pos_team_id != dplyr::lag(pos_team_id) | (pbp_version == 'V1' & grepl(X2,' Made')),
    # pos_team_change = dplyr::case_when(is.na(pos_team_change) ~ FALSE,
    #                                    T ~ pos_team_change),
    # pos_number = cumsum(pos_team_change[!is.na(pos_team_change)]) + 1,
    desc = paste(X2, X4),
    # shot descriptors
    is_shot = grepl('2pt|3pt|freethrow| Two Point| Three Point| Free Throw| Layup| Tip In| Dunk',
                    desc),
    field_goal_attempt = grepl('2pt|3pt| Two Point| Three Point| Layup| Tip In| Dunk',
                               desc),
    two_point_attempt = grepl('2pt| Two Point| Layup| Tip In| Dunk',
                              desc),
    three_point_attempt = grepl('3pt| Three Point',
                                desc),
    jumpshot = grepl('jumpshot| Jumper',
                     desc),
    pullup_jumpshot = grepl('pullupjumpshot',
                            desc),
    floating_jumpshot = grepl('floatingjumpshot',
                              desc),
    stepback_jumpshot = grepl('stepbackjumpshot',
                              desc),
    turnaround_jumpshot = grepl('turnaroundjumpshot',
                                desc),
    layup = grepl('layup| Layup',
                  desc),
    driving_layup = grepl('drivinglayup',
                          desc),
    dunk = grepl('dunk| Dunk',
                 desc),
    hookshot = grepl('hookshot| Tip In',
                     desc),
    alleyoop = grepl('alleyoop| Alleyoop',
                     desc),
    points_in_the_paint = grepl('pointsinthepaint',
                                desc),
    from_turnover = grepl('fromturnover',
                          desc),
    fast_break = grepl('fastbreak',
                       desc),
    second_chance = grepl('2ndchance',
                          desc),
    assist = grepl('assist| Assist',
                   desc),
    block = grepl('block| Block',
                  desc),
    free_throw = grepl('freethrow| Free Throw',
                       desc),
    one_and_one = grepl('oneandone',
                        desc),
    # foul descriptors
    foul = grepl('foul| Foul',
                 desc),
    personal_foul = grepl('foul personal',
                          desc),
    shooting_foul = grepl('foul personal shooting',
                          desc),
    offensive_foul = grepl('foul offensive',
                           desc),
    technical_foul = grepl('foul technical',
                           desc),
    technical_foul_class_a = grepl('foul technical classa',
                                   desc),
    technical_foul_class_b = grepl('foul technical classb',
                                   desc),
    flagrant_foul = grepl('flagrant',
                          desc),
    flagrant_foul_one = grepl('flagrant1',
                              desc),
    flagrant_foul_two = grepl('flagrant2',
                              desc),
    loose_ball_foul = grepl('loose ball',
                            desc),
    double_foul = grepl('foul double personal',
                        desc),
    foul_on_coach = grepl('foul coach',
                          desc),
    foul_on_bench = grepl('foul bench',
                          desc),
    # rebounding descriptors
    rebound = grepl('rebound| Rebound',
                    desc),
    offensive_rebound = grepl('rebound offensive| Offensive Rebound',
                              desc),
    defensive_rebound = grepl('rebound defensive| Defensive Rebound',
                              desc),
    dead_ball_rebound = grepl('dead ball rebound| Deadball Rebound',
                              desc),
    team_rebound = grepl('rebound offensive team|rebound defensive team|TEAM Offensive Rebound|TEAM Defensive Rebound',
                         desc),
    # turnover descriptors
    turnover = grepl('turnover| Turnover',
                     desc),
    bad_pass_turnover = grepl('bad pass',
                              desc),
    lost_ball_turnover = grepl('lost ball',
                               desc),
    travel = grepl('travel',
                   desc),
    dribbling_violation = grepl('dribbling violation',
                                desc),
    three_second_violation = grepl('3 seconds',
                                   desc),
    offensive_goaltending = grepl('offensive goal tending',
                                  desc),
    lane_violation = grepl('lane violation',
                           desc),
    out_of_bounds = grepl('out of bounds',
                          desc),
    # other plays
    steal = grepl('steal| Steal',
                  desc),
    jump_ball = grepl('jumpball',
                      desc),
    substitution = grepl('substitution| Enters Game| Leaves Game', desc),
    substitution_in = grepl('substitution in| Enters Game', desc),
    substitution_out = grepl('substitution out| Leaves Game', desc),
    away_score = stringi::stri_extract_all_regex(X3, '[0-9]*-') |>
      lapply(
        stringi::stri_extract_all_regex,
        pattern = '[0-9]*',
        simplify = T
      ) |>
      lapply(as.numeric) |>
      lapply(max, na.rm = T) |>
      unlist(),
    home_score = stringi::stri_extract_all_regex(X3, '-[0-9]*') |>
      lapply(
        stringi::stri_extract_all_regex,
        pattern = '[0-9]*',
        simplify = T
      ) |>
      lapply(as.numeric) |>
      lapply(max, na.rm = T) |>
      unlist(),
    away_score = dplyr::case_when(
      dplyr::row_number() == 1 ~ as.integer(0),
      is.infinite(away_score) ~ NA_integer_,
      T ~ as.integer(away_score)
    ),
    home_score = dplyr::case_when(
      dplyr::row_number() == 1 ~ as.integer(0),
      is.infinite(home_score) ~ NA_integer_,
      T ~ as.integer(home_score)
    ),
    ordering = dplyr::case_when(
      turnover ~ 1,
      field_goal_attempt ~ 2,
      assist ~ 3,
      foul ~ 4,
      substitution ~ 5,
      free_throw ~ 6,
      rebound ~ 7,
      T ~ 0
    )
  ) |>
  dplyr::arrange(ordering) |>
  dplyr::arrange(period_second,
                 period_first,
                 desc(minute),
                 desc(second),
                 desc(centiseconds)) |>
  tidyr::fill(c(home_score, away_score), .direction = "down") |>
  dplyr::mutate(
    points_on_play = (away_score - dplyr::lag(away_score)) + (home_score - dplyr::lag(home_score)),
    potential_points_on_play = stringi::stri_count_regex(desc, '2pt') * 2 + stringi::stri_count_regex(desc, '3pt') * 3 + stringi::stri_count_regex(desc, '1freethrow') + stringi::stri_count_regex(desc, '2freethrow') * 2 + stringi::stri_count_regex(desc, '3freethrow') * 3,
    player_on_play = dplyr::case_when(pbp_version == 'V2' ~ gsub(',', '', stringi::stri_extract_all_regex(desc, '(.*),')),
                                      T ~
    player_on_play = trimws(
      dplyr::case_when(
        trimws(player_on_play) == 'Team' ~ NA_character_,
        T ~ player_on_play
      )
    ),
    on_court_flag = dplyr::case_when(substitution_in ~ 1,
                                     substitution_out ~ 0,
                                     T ~ NA_real_)
  ) |>
  tidyr::pivot_wider(names_from = player_on_play,
                     values_from = on_court_flag,
                     names_prefix = 'x_') |>
  dplyr::mutate(dplyr::across(
    dplyr::starts_with('x_'),
    ~ dplyr::case_when(dplyr::lead(.) == 0 ~ 1,
                       dplyr::lead(.) == 1 ~ 0,
                       T ~
                         .)
  )) |>
  tidyr::fill(c(dplyr::starts_with('x_')), .direction = "down") |>
  tidyr::fill(c(dplyr::starts_with('x_')), .direction = "up") |>
  (\(a) cbind(a, a |>
                (
                  \(d)
                  d |>
                    dplyr::select(dplyr::starts_with('x_')) |>
                    dplyr::select(-dplyr::contains('x_NA')) |>
                    (
                      \(f) which(f == 1, arr.ind = T) |>
                        as.data.frame() |>
                        dplyr::group_by(row) |>
                        dplyr::summarize(on_court = paste0(colnames(f)[unlist(list(col))], collapse =
                                                             ','))
                    )()
                )()))() |>
  dplyr::filter(!substitution) |>
  tidyr::separate(col = 'on_court',
                  sep = ',',
                  into = paste0('player_', c(1:10))) |>
  dplyr::mutate(
    across(player_1:player_10, ~ gsub('x_', '', .)),
    across(
      player_1:player_10,
      ~ plyr::mapvalues(
        .,
        from = rosters$player,
        to = rosters$player_id,
        warn_missing = F
      ),
      .names = '{.col}_id'
    ),
    across(
      player_1:player_10,
      ~ plyr::mapvalues(
        .,
        from = rosters$player,
        to = rosters$team_id,
        warn_missing = F
      ),
      .names = '{.col}_team_id'
    ),
    across(
      player_1:player_10,
      ~ plyr::mapvalues(
        .,
        from = rosters$player,
        to = rosters$pos,
        warn_missing = F
      ),
      .names = '{.col}_pos'
    )
  ) -> tmp
