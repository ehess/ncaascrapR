#' @title ncaa_bb_pbp
#' @description Scrapes and parses pbp data for ncaa basketball games
#'
#' @param game_id the game_id for the specified game from stats.ncaa.com
#' @return a dataframe of cleaned, parsed play-by-play data for the specified
#' `game_id`
ncaa_bb_pbp <- function(game_id) {
  html <- # grab html here to avoid repeatedly referencing it
    paste0('https://stats.ncaa.org/game/play_by_play/', game_id) |>
    rvest::read_html()
  box_html <- # grab box html for name joins later
    paste0('https://stats.ncaa.org/game/box_score/', game_id) |>
    rvest::read_html()
  rosters <-
    box_html |> # generate a roster dataframe for both teams
    rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
    rvest::html_table() |>
    janitor::row_to_names(2) |>
    dplyr::filter(Player != 'Totals') |>
    dplyr::mutate(
      player = gsub(',', '', sub("(^.*),\\s(.*$)", "\\2 \\1", Player)),
      # extract and parse player names
      player_id = box_html |> # grab NCAA player IDs
        rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
        rvest::html_nodes('a') |>
        rvest::html_attr('href') |>
        (\(x) gsub(
          '_seq=',
          '',
          stringi::stri_extract_all_regex(x, '_seq=([0-9]*)$')
        ))() |>
        (\(x) c(x, ''))(),
      team = box_html |> # grab team
        rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
        rvest::html_node('.heading') |>
        rvest::html_text() |>
        trimws(),
      team_id = box_html |> # grab ncaa team ID
        rvest::html_nodes(xpath = '/html/body/div[2]/table[1]/tr/td/a') |>
        rvest::html_attr('href') |>
        (\(x) gsub('/teams/', '', x))() |>
        dplyr::nth(1),
      pos = Pos
    ) |>
    dplyr::select(player:pos) |> # select relevant cols
    dplyr::bind_rows(
      # bind to other team's roster DF
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
    ) |>
    dplyr::mutate(player = toupper(gsub('\\.', '', player)))

  html |> # fun times start here!
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
    tidyr::separate(X1,
                    c('minute', 'second', 'centiseconds'),
                    fill = "right",
                    extra = "drop") |>
    dplyr::mutate(
      pbp_version = ifelse(
        # we need to check for the PBP version, there are two different kinds, this affects things like regex
        html |>
          rvest::html_node(xpath = 'body/div[2]/table[6]/tr[2]/td[2]') |>
          rvest::html_text() |>
          trimws() %in% c('jumpball startperiod', 'period start', 'game start'),
        'V2',
        'V1'
      )
    ) |>
    dplyr::filter(grepl('([0-9]*)-([0-9]*)', X3)) |> # remove any filler rows indicating things like "jumpball start"
    dplyr::mutate(
      # parse time of game
      across(c(minute, second, centiseconds), as.numeric),
      centiseconds = dplyr::case_when(is.na(centiseconds) ~ 0,
                                      T ~ centiseconds),
      milliseconds = centiseconds * 10,
      time = lubridate::period(minute = minute, second = second) + lubridate::milliseconds(milliseconds),
      time = dplyr::case_when(X3 == 'Score' ~ lubridate::period(minute = 20, second = 0),
                              T ~ time)
    ) |>
    tidyr::separate(period,
                    c('period_first', 'period_second'),
                    fill = "right",
                    extra = "drop") |>
    dplyr::arrange(period_second, period_first, desc(time)) |> # :sickos: fun sorting stuff here
    dplyr::ungroup() |>
    dplyr::mutate(
      period = paste(period_first, period_second),
      play_id = as.character(paste0(game_id, dplyr::row_number())),
      # unique identifier for each play
      game_id = game_id,
      game_start_datetime = html |> # time of game
        rvest::html_node(xpath = '/html/body/div[2]/table[3]/tr[1]/td[2]') |>
        rvest::html_text(trim = T) |>
        lubridate::mdy_hm(tz = 'America/New_York', quiet = T),
      game_start_datetime = dplyr::case_when(
        is.na(game_start_datetime) ~ html |>
          rvest::html_node(xpath = '/html/body/div[2]/table[3]/tr[1]/td[2]') |>
          rvest::html_text(trim = T) |>
          lubridate::mdy(tz = 'America/New_York', quiet = T),
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
            lubridate::mdy_hm(tz = 'America/New_York', quiet = T)
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
        (\(x) gsub('/teams/', '', x))() |>
        as.character(),
      home_team_id = html |>
        rvest::html_node('body') |>
        rvest::html_node('div#contentarea') |>
        rvest::html_node('table.mytable') |>
        rvest::html_nodes('tr') |>
        rvest::html_nodes('a') |>
        rvest::html_attr('href') |>
        dplyr::nth(1) |>
        (\(x) gsub('/teams/', '', x))() |>
        as.character(),
      #' check for which team has possession on each play -- this is effectively
      #' the instantaneous "who has possession" whenever a given stat is recorded
      #' e.g. if team A makes a shot, that shot is recorded as the possession of
      #' team A, not team B who has possession immediately after the made shot
      #' however, if team B forces a steal, team B is recorded as the possessing
      #' team
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
      #' fixing a very annoying edge case where a team wins jump ball
      #' then immediately loses it on a turnover
      pos_team = dplyr::case_when(
        pbp_version[1] == 'V1' &
          grepl(dplyr::lead(X2[1]), ' Turnover') ~ home_team,
        pbp_version[1] == 'V1' &
          grepl(dplyr::lead(X4[1]), ' Turnover') ~ away_team,
        T ~ pos_team
      ),
    ) |>
    #' for when we can't guess who has possession, we basically fill in the gaps
    #' based on who had possession before and after a play
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
        period == '1st Half' ~ period_time_remaining + lubridate::ms('20:00', quiet =
                                                                       T),
        T ~ period_time_remaining
      ),
      game_seconds_remaining = dplyr::case_when(
        period == '1st Half' ~ period_seconds_remaining + 1200,
        T ~ period_seconds_remaining
      ),
      #' putting these columns on temporary hold, V1 PBP completely breaks them
      #' pos_team_change = pos_team_id != dplyr::lag(pos_team_id) | (pbp_version == 'V1' & grepl(X2,' Made')),
      #' pos_team_change = dplyr::case_when(is.na(pos_team_change) ~ FALSE,
      #'                                    T ~ pos_team_change),
      #' pos_number = cumsum(pos_team_change[!is.na(pos_team_change)]) + 1,
      desc = paste(X2, X4),
      # shot descriptors
      is_shot = grepl(
        '2pt|3pt|freethrow| Two Point| Three Point| Free Throw| Layup| Tip In| Dunk',
        desc
      ),
      field_goal_attempt = grepl('2pt|3pt| Two Point| Three Point| Layup| Tip In| Dunk',
                                 desc),
      two_point_attempt = grepl('2pt| Two Point| Layup| Tip In| Dunk',
                                desc),
      three_point_attempt = grepl('3pt| Three Point',
                                  desc),
      jumpshot = grepl('jumpshot| Jumper',
                       desc),
      #' these are deliberately commented out for a few reasons:
      #' 1. As stringer data, these designators are somewhat noisy
      #' 2. As Seth Partnow pointed out in The Midrange Theory, these designators can be biased
      #' 3. THese designators are only available in V2 of the PBP, not V1
      # pullup_jumpshot = grepl('pullupjumpshot',
      #                         desc),
      # floating_jumpshot = grepl('floatingjumpshot',
      #                           desc),
      # stepback_jumpshot = grepl('stepbackjumpshot',
      #                           desc),
      # turnaround_jumpshot = grepl('turnaroundjumpshot',
      #                             desc),
      layup = grepl('layup| Layup',
                    desc),
      # driving_layup = grepl('drivinglayup',
      #                       desc),
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
      dead_ball_rebound = grepl('dead ball rebound| Deadball Rebound|deadball',
                                desc),
      team_rebound = grepl(
        'rebound offensive team|rebound defensive team|TEAM Offensive Rebound|TEAM Defensive Rebound',
        desc
      ),
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
      timeout = grepl(' timeout', desc),
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
      total = away_score + home_score,
      ordering = dplyr::case_when(
        timeout ~ 1,
        turnover ~ 2,
        field_goal_attempt ~ 3,
        assist ~ 4,
        foul ~ 5,
        substitution ~ 6,
        free_throw ~ 7,
        rebound ~ 8,
        T ~ 0
      )
    ) |>
    dplyr::arrange(total) |>
    # estimate number of points on play by looking at total score prior -> total score after
    dplyr::mutate(points_on_play = (away_score - dplyr::lag(away_score)) + (home_score - dplyr::lag(home_score))) |>
    # we do this odd ordering bit to make the substitutions a little easier
    dplyr::arrange(period_second,
                   period_first,
                   desc(minute),
                   desc(second),
                   desc(centiseconds),
                   ordering) |>
    dplyr::group_by(time) |>
    dplyr::mutate(has_fts = any(free_throw)) |>
    dplyr::ungroup() |>
    dplyr::filter(!(has_fts & dead_ball_rebound)) |>
    tidyr::fill(c(home_score, away_score), .direction = "down") |>
    dplyr::mutate(
      potential_points_on_play = as.integer(
        stringi::stri_count_regex(desc, '2pt| Two Point| Layup| Tip In| Dunk') * 2 + stringi::stri_count_regex(desc, '3pt| Three Point') * 3 + stringi::stri_count_regex(desc, ' freethrow| Free Throw')
      ),
      # extract player on play for lineups and joining
      player_on_play = trimws(
        dplyr::case_when(
          pbp_version == 'V2' ~ gsub(',', '', stringi::stri_extract_all_regex(desc, '(.*),')),
          pbp_version == 'V1' &
            stringi::stri_detect_regex(desc, '(\\b[A-Z]+,\\b[A-Z]+ )') ~ stringi::stri_extract_all_regex(desc, '(\\b[A-Z]+,\\b[A-Z]+ )', simplify = T),
          T ~  stringi::stri_extract_all_regex(desc, '\\b[A-Z]+ \\b[A-Z]+ \\b[A-Z]+ ', simplify = T)
        )
      ),
      player_on_play = trimws(
        dplyr::case_when(
          toupper(trimws(player_on_play)) == 'TEAM' ~ NA_character_,
          technical_foul ~ NA_character_,
          stringi::stri_detect_regex(player_on_play, ',') &
            pbp_version == 'V2' ~ gsub(',', '', sub(
              "(^.*),\\s(.*$)", "\\2 \\1", player_on_play
            )),
          stringi::stri_detect_regex(player_on_play, ',') &
            pbp_version == 'V1' ~ gsub(',', '', sub(
              "(^.*),(.*$)", "\\2 \\1", player_on_play
            )),
          T ~ player_on_play
        )
      ),
      player_on_play = gsub('\\.', '', toupper(player_on_play)),
      # add a flag whenever a player joins or leaves the game
      on_court_flag = dplyr::case_when(substitution_in ~ 1,
                                       substitution_out ~ 0,
                                       T ~ NA_real_)
    ) |>
    #' grouping by period because some lineups will change between periods
    #' without being noted in the pbp
    dplyr::group_by(period) |>
    dplyr::group_modify( ~ (
      \(a) cbind(
        a,
        a |>
          # create a matrix of all players mentioned in the pbp at any point
          tidyr::pivot_wider(
            names_from = player_on_play,
            values_from = on_court_flag,
            names_prefix = 'x_'
          ) |>
          #' whenever a player is subbed in, they have a 1 in that row and a 0
          #' in the row before. whenever a player is subbed out, they have a 0
          #' in that row and a 1 in the row before
          dplyr::mutate(dplyr::across(
            dplyr::starts_with('x_'),
            ~ dplyr::case_when(dplyr::lead(.) == 0 ~ 1,
                               dplyr::lead(.) == 1 ~ 0,
                               T ~
                                 .)
          )) |>
          #' we then cascade the 1s and 0s up and down to create map of who is
          #' in the game at any given time
          tidyr::fill(c(dplyr::starts_with('x_')), .direction = "down") |>
          tidyr::fill(c(dplyr::starts_with('x_')), .direction = "up") |>
          dplyr::mutate(dplyr::across(
            dplyr::starts_with('x_'),
            ~ dplyr::case_when(is.na(.x) ~ 1,
                               T ~ .x)
          )) |>
          # finally we collapse the values down into a string of who is on court
          (
            \(d)
            d |>
              dplyr::select(dplyr::starts_with('x_')) |>
              dplyr::select(-dplyr::any_of('x_NA')) |>
              (
                \(f) which(f == 1, arr.ind = T) |>
                  as.data.frame() |>
                  dplyr::group_by(row) |>
                  dplyr::summarize(on_court = paste0(colnames(f)[unlist(list(col))], collapse =
                                                       ','))
              )()
          )()
      )
    )(.x)) |>
    dplyr::ungroup() |>
    # we don't need any substitution data anymore, buhbye!
    dplyr::filter(!substitution) |>
    # separating the lineup string -- this should _only_ be 10 players
    tidyr::separate(col = 'on_court',
                    sep = ',',
                    into = paste0('player_', c(1:10))) |>
    #' now we map player names to the roster df. this _could_ be noisy in theory
    #' but i haven't seen any issues in practice
    dplyr::mutate(
      across(dplyr::matches('player_([0-9]*)$'), ~ gsub('x_', '', .)),
      across(
        dplyr::matches('player_([0-9]*)$'),
        ~ plyr::mapvalues(
          .,
          from = rosters$player,
          to = rosters$player_id,
          warn_missing = F
        ),
        .names = '{.col}_id'
      ),
      across(
        dplyr::matches('player_([0-9]*)$'),
        ~ plyr::mapvalues(
          .,
          from = rosters$player,
          to = rosters$team_id,
          warn_missing = F
        ),
        .names = '{.col}_team_id'
      ),
      across(
        dplyr::matches('player_([0-9]*)$'),
        ~ plyr::mapvalues(
          .,
          from = rosters$player,
          to = rosters$pos,
          warn_missing = F
        ),
        .names = '{.col}_pos'
      ),
      player_on_play_id = plyr::mapvalues(
        player_on_play,
        from = rosters$player,
        to = rosters$player_id,
        warn_missing = F
      ),
      player_on_play_team_id = plyr::mapvalues(
        player_on_play,
        from = rosters$player,
        to = rosters$team_id,
        warn_missing = F
      ),
      player_on_play_pos = plyr::mapvalues(
        player_on_play,
        from = rosters$player,
        to = rosters$pos,
        warn_missing = F
      ),
    ) |>
    #' some character encoding stuff, dropping players with mispelled names in
    #' the pbp
    dplyr::mutate(
      dplyr::across(
        dplyr::matches('player_(.*)_id'),
        ~ dplyr::case_when(
          stringi::stri_detect_regex('[A-z]', .x) ~ NA_character_,
          is.na(.x) ~ NA_character_,
          T ~ as.character(.x)
        )
      ),
      dplyr::across(
        dplyr::ends_with('_pos'),
        ~ dplyr::case_when(nchar(.x) > 1 ~ NA_character_,
                           is.na(.x) ~ NA_character_,
                           T ~ .x)
      ),
      # fixing any plays missing points
      points_on_play = dplyr::case_when(is.na(points_on_play) ~ as.integer(0),
                                        T ~ points_on_play),
      desc = trimws(desc),
      #' for v1, these columns are not recorded, so they are set to NA so they
      #' don't register as false negatives
      dplyr::across(
        c(
          points_in_the_paint,
          from_turnover,
          fast_break,
          second_chance,
          one_and_one,
          personal_foul,
          shooting_foul,
          offensive_foul,
          technical_foul,
          technical_foul_class_a,
          technical_foul_class_b,
          flagrant_foul,
          flagrant_foul_one,
          flagrant_foul_two,
          loose_ball_foul,
          double_foul,
          foul_on_coach,
          foul_on_bench,
          bad_pass_turnover,
          lost_ball_turnover,
          travel,
          dribbling_violation,
          three_second_violation,
          offensive_goaltending,
          lane_violation,
          out_of_bounds,
          jump_ball
        ),
        ~ dplyr::case_when(pbp_version == 'V1' ~ NA,
                           T ~ .x)
      )
    ) |>
    # selecting relevant and cleaned columns
    dplyr::select(
      game_id,
      pbp_version,
      game_start_datetime,
      game_start_date,
      game_start_time,
      home_team,
      away_team,
      home_team_id,
      away_team_id,
      period,
      play_id,
      home_score,
      away_score,
      points_on_play,
      potential_points_on_play,
      pos_team:jump_ball,
      player_on_play,
      player_on_play_id,
      player_on_play_team_id,
      player_on_play_pos,
      starts_with('player_')
    ) |>
    # some last column spec cleaning
    dplyr::mutate(
      dplyr::across(dplyr::ends_with('_id'), as.character),
      dplyr::across(dplyr::ends_with('_pos'), ~ factor(.x, levels = c('G', 'F', 'C')))
    )
}
