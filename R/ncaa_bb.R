#' @title ncaa_bb_box_html
#' @description helper function to grab the raw html for a given game's box
#' @param game_id a unique identifier for an NCAA BB game
#' @returns an rvest html object
ncaa_bb_box_html <- function(game_id) {
  paste0('https://stats.ncaa.org/game/box_score/', game_id) |>
    rvest::read_html()
}

#' @title ncaa_bb_pbp_html
#' @description helper function to grab the raw html for a given game's pbp
#' @param game_id a unique identifier for an NCAA bb game
#' @returns an rvest html object
ncaa_bb_pbp_html <- function(game_id) {
  paste0('https://stats.ncaa.org/game/play_by_play/', game_id) |>
    rvest::read_html()
}

#' @title ncaa_bb_calc_team_box_from_pbp
#' @description calculates team box score stats from pbp dataframe for ncaa
#' basketball games
#' @param pbp a dataframe of pbp data created by `ncaa_bb_pbp`
#' @return a dataframe formatted identically to the data returned by `ncaa_bb_team_box`
ncaa_bb_calc_team_box_from_pbp <- function(pbp) {
  pbp |>
    dplyr::filter(!is.na(player_on_play_team_id)) |>
    dplyr::group_by(team_id = player_on_play_team_id) |>
    dplyr::summarize(
      fgm = sum(field_goal_made),
      fga = sum(field_goal_attempt),
      tpm = sum(three_point_made),
      tpa = sum(three_point_attempt),
      ftm = sum(free_throw_made),
      fta = sum(free_throw_attempt),
      pts = sum(points_on_play),
      oreb = sum(offensive_rebound),
      dreb = sum(defensive_rebound),
      treb = oreb + dreb,
      ast = sum(assist),
      to = sum(turnover),
      stl = sum(steal),
      blk = sum(block)
    ) |>
    dplyr::arrange(team_id)
}

#' @title ncaa_bb_player_box_parse
#' @description a function to parse box html into a player box score table
#' @param box_html an rvest html object
#' @return a dataframe of cleaned, parsed player box data for the specified
#' `game_id`
ncaa_bb_player_box_parse <- function(box_html) {
  game_id <- box_html |>
    rvest::html_node(xpath = '/html/body/div[2]/div[3]/div/div/ul/li[3]/a') |>
    rvest::html_attr('href') |>
    (\(x) gsub('/game/play_by_play/', '', x))()
  player_id_a <- box_html |>
    rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
    rvest::html_nodes('a') |>
    rvest::html_attr('href') |>
    (\(x) gsub(
      '_seq=',
      '',
      stringi::stri_extract_all_regex(x, '_seq=([0-9]*)$')
    ))() |>
    (\(x) c(x, ''))()
  length(player_id_a) <- box_html |>
    rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
    rvest::html_table() |>
    janitor::row_to_names(2) |>
    dplyr::filter(Player != 'Totals') |>
    nrow()
  player_id_b <- box_html |>
    rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
    rvest::html_nodes('a') |>
    rvest::html_attr('href') |>
    (\(x) gsub(
      '_seq=',
      '',
      stringi::stri_extract_all_regex(x, '_seq=([0-9]*)$')
    ))()  |>
    (\(x) c(x, ''))()
  length(player_id_b) <- box_html |>
    rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
    rvest::html_table() |>
    janitor::row_to_names(2) |>
    dplyr::filter(Player != 'Totals') |>
    nrow()
  box_html |>
    rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
    rvest::html_table() |>
    janitor::row_to_names(2) |>
    dplyr::filter(Player != 'Totals') |>
    dplyr::rename_with(\(x) "MP", dplyr::starts_with("Min")) |>
    dplyr::rename_with(\(x) "ORebs", dplyr::starts_with("Off Reb")) |>
    dplyr::rename_with(\(x) "DRebs", dplyr::starts_with("Def Reb")) |>
    dplyr::rename_with(\(x) "STL", dplyr::starts_with("ST")) |>
    dplyr::rename_with(\(x) "BLK", dplyr::starts_with("BLKS")) |>
    dplyr::rename_with(\(x) "Fouls", dplyr::starts_with("PF")) |>
    dplyr::mutate(
      game_id = game_id,
      player = gsub(',', '', sub("(^.*),\\s(.*$)", "\\2 \\1", Player)),
      player_id = player_id_a,
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
      across(.cols = everything(), ~ gsub("\\*|\\-|\\/", "", .x)),
      pos = Pos,
      min = lubridate::ms(MP, roll = T, quiet = T),
      fgm = as.integer(FGM),
      fga = as.integer(FGA),
      fgpct = dplyr::case_when(is.na(fga) ~ NaN, is.na(fgm) ~ 0, T ~ fgm / fga),
      tpm = as.integer(`3FG`),
      tpa = as.integer(`3FGA`),
      tppct = dplyr::case_when(is.na(tpa) ~ NaN, is.na(tpm) ~ 0, T ~ tpm / tpa),
      ftm = as.integer(FT),
      fta = as.integer(FTA),
      ftpct = dplyr::case_when(is.na(fta) ~ NaN, is.na(ftm) ~ 0, T ~ ftm / fta),
      pts = as.integer(PTS),
      oreb = as.integer(ORebs),
      dreb = as.integer(DRebs),
      treb = as.integer(`Tot Reb`),
      ast = as.integer(AST),
      to = as.integer(TO),
      stl = as.integer(STL),
      blk = as.integer(BLK),
      pf = as.integer(gsub('/', '', Fouls)),
      # dq = as.integer(gsub('/', '', DQ)),
      # tf = as.integer(gsub('/', '', `Tech Fouls`)),
      dplyr::across(
        where(is.integer),
        ~ dplyr::case_when(is.na(.x) ~ as.integer(0),
                           T ~
                             .x)
      )
    ) |>
    dplyr::select(player:pf) |>
    dplyr::bind_rows(
      box_html |>
        rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
        rvest::html_table() |>
        janitor::row_to_names(2) |>
        dplyr::filter(Player != 'Totals') |>
        dplyr::rename_with(\(x) "MP", dplyr::starts_with("Min")) |>
        dplyr::rename_with(\(x) "ORebs", dplyr::starts_with("Off Reb")) |>
        dplyr::rename_with(\(x) "DRebs", dplyr::starts_with("Def Reb")) |>
        dplyr::rename_with(\(x) "STL", dplyr::starts_with("ST")) |>
        dplyr::rename_with(\(x) "BLK", dplyr::starts_with("BLKS")) |>
        dplyr::rename_with(\(x) "Fouls", dplyr::starts_with("PF")) |>
        dplyr::mutate(
          game_id = game_id,
          player = gsub(',', '', sub("(^.*),\\s(.*$)", "\\2 \\1", Player)),
          player_id = player_id_b,
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
          across(.cols = everything(), ~ gsub("\\*|\\-|\\/", "", .x)),
          pos = Pos,
          min = lubridate::ms(MP, roll = T, quiet =
                                T),
          fgm = as.integer(FGM),
          fga = as.integer(FGA),
          fgpct = dplyr::case_when(is.na(fga) ~ NaN, is.na(fgm) ~ 0, T ~ fgm / fga),
          tpm = as.integer(`3FG`),
          tpa = as.integer(`3FGA`),
          tppct = dplyr::case_when(is.na(tpa) ~ NaN, is.na(tpm) ~ 0, T ~ tpm / tpa),
          ftm = as.integer(FT),
          fta = as.integer(FTA),
          ftpct = dplyr::case_when(is.na(fta) ~ NaN, is.na(ftm) ~ 0, T ~ ftm / fta),
          pts = as.integer(PTS),
          oreb = as.integer(ORebs),
          dreb = as.integer(DRebs),
          treb = as.integer(`Tot Reb`),
          ast = as.integer(AST),
          to = as.integer(TO),
          stl = as.integer(STL),
          blk = as.integer(BLK),
          pf = as.integer(gsub('/', '', Fouls)),
          # dq = as.integer(gsub('/', '', DQ)),
          # tf = as.integer(gsub('/', '', `Tech Fouls`)),
          dplyr::across(
            where(is.integer),
            ~ dplyr::case_when(is.na(.x) ~ as.integer(0),
                               T ~
                                 .x)
          ),
          dplyr::across(
            dplyr::ends_with('pct'),
            ~ dplyr::case_when(is.na(.x) ~ NaN,
                               T ~
                                 .x)
          )
        ) |>
        dplyr::select(player:pf)
    )
}

#' @title ncaa_bb_team_box_parse
#' @description a function to parse box html into a team box score table
#' @param box_html an rvest html object
#' @return a dataframe of cleaned, parsed team box data for the specified
#' `game_id`
ncaa_bb_team_box_parse <- function(box_html) {
  game_id <- box_html |>
    rvest::html_node(xpath = '/html/body/div[2]/div[3]/div/div/ul/li[3]/a') |>
    rvest::html_attr('href') |>
    (\(x) gsub('/game/play_by_play/', '', x))()
  box_html |>
    rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
    rvest::html_table() |>
    janitor::row_to_names(2) |>
    dplyr::filter(Player == 'Totals') |>
    dplyr::rename_with(\(x) "MP", dplyr::starts_with("Min")) |>
    dplyr::rename_with(\(x) "ORebs", dplyr::starts_with("Off Reb")) |>
    dplyr::rename_with(\(x) "DRebs", dplyr::starts_with("Def Reb")) |>
    dplyr::rename_with(\(x) "STL", dplyr::starts_with("ST")) |>
    dplyr::rename_with(\(x) "BLK", dplyr::starts_with("BLKS")) |>
    dplyr::rename_with(\(x) "Fouls", dplyr::starts_with("PF")) |>
    dplyr::mutate(
      game_id = game_id,
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
      across(.cols = everything(), ~ gsub("\\*|\\-|\\/", "", .x)),
      min = lubridate::ms(MP, roll = T, quiet = T),
      fgm = as.integer(FGM),
      fga = as.integer(FGA),
      fgpct = dplyr::case_when(is.na(fga) ~ NaN, is.na(fgm) ~ 0, T ~ fgm / fga),
      tpm = as.integer(`3FG`),
      tpa = as.integer(`3FGA`),
      tppct = dplyr::case_when(is.na(tpa) ~ NaN, is.na(tpm) ~ 0, T ~ tpm / tpa),
      ftm = as.integer(FT),
      fta = as.integer(FTA),
      ftpct = dplyr::case_when(is.na(fta) ~ NaN, is.na(ftm) ~ 0, T ~ ftm / fta),
      pts = as.integer(PTS),
      oreb = as.integer(ORebs),
      dreb = as.integer(DRebs),
      treb = as.integer(`Tot Reb`),
      ast = as.integer(AST),
      to = as.integer(TO),
      stl = as.integer(STL),
      blk = as.integer(BLK),
      pf = as.integer(gsub('/', '', Fouls)),
      # dq = as.integer(gsub('/', '', DQ)),
      # tf = as.integer(gsub('/', '', `Tech Fouls`)),
      dplyr::across(
        where(is.integer),
        ~ dplyr::case_when(is.na(.x) ~ as.integer(0),
                           T ~
                             .x)
      ),
      dplyr::across(
        dplyr::ends_with('pct'),
        ~ dplyr::case_when(is.na(.x) ~ NaN,
                           T ~
                             .x)
      )
    ) |>
    dplyr::select(team:pf) |>
    dplyr::bind_rows(
      box_html |>
        rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
        rvest::html_table() |>
        janitor::row_to_names(2) |>
        dplyr::filter(Player == 'Totals') |>
        dplyr::rename_with(\(x) "MP", dplyr::starts_with("Min")) |>
        dplyr::rename_with(\(x) "ORebs", dplyr::starts_with("Off Reb")) |>
        dplyr::rename_with(\(x) "DRebs", dplyr::starts_with("Def Reb")) |>
        dplyr::rename_with(\(x) "STL", dplyr::starts_with("ST")) |>
        dplyr::rename_with(\(x) "BLK", dplyr::starts_with("BLKS")) |>
        dplyr::rename_with(\(x) "Fouls", dplyr::starts_with("PF")) |>
        dplyr::mutate(
          game_id = game_id,
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
          across(.cols = everything(), ~ gsub("\\*|\\-|\\/", "", .x)),
          min = lubridate::ms(MP, roll = T, quiet =
                                T),
          fgm = as.integer(FGM),
          fga = as.integer(FGA),
          fgpct = dplyr::case_when(is.na(fga) ~ NaN, is.na(fgm) ~ 0, T ~ fgm / fga),
          tpm = as.integer(`3FG`),
          tpa = as.integer(`3FGA`),
          tppct = dplyr::case_when(is.na(tpa) ~ NaN, is.na(tpm) ~ 0, T ~ tpm / tpa),
          ftm = as.integer(FT),
          fta = as.integer(FTA),
          ftpct = dplyr::case_when(is.na(fta) ~ NaN, is.na(ftm) ~ 0, T ~ ftm / fta),
          pts = as.integer(PTS),
          oreb = as.integer(ORebs),
          dreb = as.integer(DRebs),
          treb = as.integer(`Tot Reb`),
          ast = as.integer(AST),
          to = as.integer(TO),
          stl = as.integer(STL),
          blk = as.integer(BLK),
          pf = as.integer(gsub('/', '', Fouls)),
          # dq = as.integer(gsub('/', '', DQ)),
          # tf = as.integer(gsub('/', '', `Tech Fouls`)),
          dplyr::across(
            where(is.integer),
            ~ dplyr::case_when(is.na(.x) ~ as.integer(0),
                               T ~
                                 .x)
          ),
          dplyr::across(
            dplyr::ends_with('pct'),
            ~ dplyr::case_when(is.na(.x) ~ NaN,
                               T ~
                                 .x)
          )
        ) |>
        dplyr::select(team:pf)
    )
}

#' @title ncaa_bb_pbp_parse
#' @description a function to parse the box and pbp html into a pbp table
#' @param box_html an rvest html object
#' @param pbp_html an rvest html object
#' @return a dataframe of cleaned, parsed, play-by-play data for the specified
#' `game_id`
ncaa_bb_pbp_parse <- function(box_html, pbp_html) {
  game_id <- pbp_html |>
    rvest::html_node(xpath = '/html/body/div[2]/div[3]/div/div/ul/li[1]/ul/li[3]/a') |>
    rvest::html_attr('href') |>
    (\(x) gsub('/game/box_score/', '', x))()
  rosters <-
    data.frame(
      player = if(length(
        box_html |> # grab NCAA player IDs
        rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
        rvest::html_nodes('a')
      ) > 0){
        box_html |> # grab NCAA player IDs
          rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
          rvest::html_nodes('a') |>
          rvest::html_text() |>
          (\(x) gsub(
            ',', '', sub("(^.*),\\s(.*$)", "\\2 \\1", x)
          ))() |>
          stringi::stri_trans_general(id = "Latin-ASCII")
      } else {
        ''
      },
      player_id =  if(length(
        box_html |> # grab NCAA player IDs
        rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
        rvest::html_nodes('a')
      ) > 0) {
        box_html |> # grab NCAA player IDs
          rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
          rvest::html_nodes('a') |>
          rvest::html_attr('href') |>
          (\(x) gsub(
            '_seq=',
            '',
            stringi::stri_extract_all_regex(x, '_seq=([0-9]*)$')
          ))()
      } else {
        ''
      },
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
      pos = if(length(
        box_html |> # grab NCAA player IDs
        rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
        rvest::html_nodes('a')) > 0){
        box_html |>
          rvest::html_node(xpath = "/html/body/div[2]/table[5]") |>
          rvest::html_nodes(xpath = 'tr/td[count(tr/th[.="$Pos"]/preceding-sibling::th)+2]') |>
          rvest::html_text() |>
          (\(x) x[(c(
            1:length(
              box_html |> # grab NCAA player IDs
                rvest::html_node(xpath = '/html/body/div[2]/table[5]') |>
                rvest::html_nodes('a') |>
                rvest::html_text()
            )
          ))])()
      } else {
        ''
      }) |>
    dplyr::bind_rows(
      data.frame(
        player = if(length(
          box_html |> # grab NCAA player IDs
          rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
          rvest::html_nodes('a')
        ) > 0){
          box_html |> # grab NCAA player IDs
            rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
            rvest::html_nodes('a') |>
            rvest::html_text() |>
            (\(x) gsub(
              ',', '', sub("(^.*),\\s(.*$)", "\\2 \\1", x)
            ))() |>
            stringi::stri_trans_general(id = "Latin-ASCII")
        } else {
          ''
        },
        player_id =  if(length(
          box_html |> # grab NCAA player IDs
          rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
          rvest::html_nodes('a')
        ) > 0) {
          box_html |> # grab NCAA player IDs
            rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
            rvest::html_nodes('a') |>
            rvest::html_attr('href') |>
            (\(x) gsub(
              '_seq=',
              '',
              stringi::stri_extract_all_regex(x, '_seq=([0-9]*)$')
            ))()
        } else {
          ''
        },
        team = box_html |> # grab team
          rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
          rvest::html_node('.heading') |>
          rvest::html_text() |>
          trimws(),
        team_id = box_html |> # grab ncaa team ID
          rvest::html_nodes(xpath = '/html/body/div[2]/table[1]/tr/td/a') |>
          rvest::html_attr('href') |>
          (\(x) gsub('/teams/', '', x))() |>
          dplyr::nth(2),
        pos = if(length(
          box_html |> # grab NCAA player IDs
          rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
          rvest::html_nodes('a')) > 0){
          box_html |>
            rvest::html_node(xpath = "/html/body/div[2]/table[6]") |>
            rvest::html_nodes(xpath = 'tr/td[count(tr/th[.="$Pos"]/preceding-sibling::th)+2]') |>
            rvest::html_text() |>
            (\(x) x[(c(
              1:length(
                box_html |> # grab NCAA player IDs
                  rvest::html_node(xpath = '/html/body/div[2]/table[6]') |>
                  rvest::html_nodes('a') |>
                  rvest::html_text()
              )
            ))])()
        } else {
          ''
        })) |>
    dplyr::mutate(player = toupper(player))
  suppressWarnings({
    pbp <- pbp_html |> # fun times start here!
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
          pbp_html |>
            rvest::html_node(xpath = 'body/div[2]/table[6]') |>
            rvest::html_table() |>
            dplyr::pull(X3) |>
            trimws() %in% c('jumpball startperiod', 'period start', 'game start') |>
            any(),
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
        time = dplyr::case_when(
          X3 == 'Score' ~ lubridate::period(minute = 20, second = 0),
          T ~ time
        )
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
        game_start_datetime = pbp_html |> # time of game
          rvest::html_node(xpath = '/html/body/div[2]/table[3]/tr[1]/td[2]') |>
          rvest::html_text(trim = T) |>
          lubridate::mdy_hm(tz = 'America/New_York', quiet = T),
        game_start_datetime = dplyr::case_when(
          is.na(game_start_datetime) ~ pbp_html |>
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
            pbp_html |>
              rvest::html_node(xpath = '/html/body/div[2]/table[3]/tr[1]/td[2]') |>
              rvest::html_text(trim = T) |>
              lubridate::mdy_hm(tz = 'America/New_York', quiet = T)
          ) ~ NA_character_,
          T ~ game_start_time
        ),
        away_team = pbp_html |>
          rvest::html_nodes(xpath = '/html/body/div[2]/table[6]/tr[1]/td[2]') |>
          rvest::html_text(),
        home_team = pbp_html |>
          rvest::html_nodes(xpath = '/html/body/div[2]/table[6]/tr[1]/td[4]') |>
          rvest::html_text(),
        away_team_id = pbp_html |>
          rvest::html_node('body') |>
          rvest::html_node('div#contentarea') |>
          rvest::html_node('table.mytable') |>
          rvest::html_nodes('tr') |>
          rvest::html_nodes('a') |>
          rvest::html_attr('href') |>
          dplyr::nth(2) |>
          (\(x) gsub('/teams/', '', x))() |>
          as.character(),
        home_team_id = pbp_html |>
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
              ' jumpball won| assist| 2pt| 3pt| rebound| freethrow| steal| turnover | jumpball lost| foul offensive',
              X2
            ) ~ away_team,
            grepl(
              ' jumpball won| assist| 2pt| 3pt| rebound| freethrow| steal| turnover | jumpball lost| foul offensive',
              X4
            ) ~ home_team,
            grepl(' block',
                  X2) ~ home_team,
            grepl(' block',
                  X4) ~ away_team,
            T ~ NA_character_
          ),
          T ~ dplyr::case_when(
            grepl(' Rebound| made| missed| Assist| Steal| Foul| Turnover', X2) ~ away_team,
            grepl(' Rebound| made| missed| Assist| Steal| Foul| Turnover', X4) ~ home_team,
            grepl(' Block', X2) ~ home_team,
            grepl(' Block', X4) ~ away_team,
            T ~ NA_character_
          )
        ),
        #' fixing a very annoying edge case where a team wins jump ball
        #' then immediately loses it on a turnover
        # pos_team = dplyr::case_when(
        #   pbp_version[1] == 'V1' &
        #     grepl(dplyr::lead(X2[1]), ' Turnover') ~ home_team,
        #   pbp_version[1] == 'V1' &
        #     grepl(dplyr::lead(X4[1]), ' Turnover') ~ away_team,
        #   T ~ pos_team
        # ),
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
        field_goal_made = grepl(" made", desc) & field_goal_attempt,
        two_point_attempt = grepl('2pt| Two Point| Layup| Tip In| Dunk',
                                  desc),
        two_point_made = grepl(" made", desc) & two_point_attempt,
        three_point_attempt = grepl('3pt| Three Point',
                                    desc),
        three_point_made = grepl(" made", desc) &
          three_point_attempt,
        jumpshot = grepl(' jumpshot| Jumper',
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
        #                            desc),
        layup = grepl(' layup| Layup',
                      desc),
        # driving_layup = grepl('drivinglayup',
        #                       desc),
        dunk = grepl(' dunk| Dunk',
                     desc),
        hookshot = grepl(' hookshot| Tip In',
                         desc),
        alleyoop = grepl(' alleyoop| Alleyoop',
                         desc),
        points_in_the_paint = grepl(' pointsinthepaint',
                                    desc),
        from_turnover = grepl(' fromturnover',
                              desc),
        fast_break = grepl(' fastbreak',
                           desc),
        second_chance = grepl(' 2ndchance',
                              desc),
        assist = grepl(' assist| Assist',
                       desc),
        block = grepl('( block$)|( block )|( Block$)|( Block )|( Blocked Shot)',
                      desc) &
          !field_goal_attempt,
        # prevents double counting blocks+
        free_throw_attempt = grepl(' freethrow| Free Throw',
                                   desc) &
          grepl(' made| missed', desc),
        free_throw_made = grepl(" made", desc) & free_throw_attempt,
        one_and_one = grepl(' oneandone',
                            desc),
        # foul descriptors
        foul = grepl(' foul | Foul',
                     desc),
        personal_foul = grepl(' foul personal',
                              desc),
        shooting_foul = grepl(' foul personal shooting',
                              desc),
        offensive_foul = grepl(' foul offensive',
                               desc),
        technical_foul = grepl(' foul technical',
                               desc),
        technical_foul_class_a = grepl(' foul technical classa',
                                       desc),
        technical_foul_class_b = grepl(' foul technical classb',
                                       desc),
        flagrant_foul = grepl(' flagrant',
                              desc),
        flagrant_foul_one = grepl(' flagrant1',
                                  desc),
        flagrant_foul_two = grepl(' flagrant2',
                                  desc),
        loose_ball_foul = grepl(' loose ball',
                                desc),
        double_foul = grepl(' foul double personal',
                            desc),
        foul_on_coach = grepl(' foul coach',
                              desc),
        foul_on_bench = grepl(' foul bench',
                              desc),
        # rebounding descriptors
        rebound = grepl(' rebound| Rebound',
                        desc),
        offensive_rebound = grepl(
          '( rebound offensive$)|( rebound offensive )| Offensive Rebound',
          desc
        ),
        defensive_rebound = grepl(
          '( rebound defensive$)|( rebound defensive )| Defensive Rebound',
          desc
        ),
        dead_ball_rebound = grepl(
          ' dead ball rebound|( rebound defensivedeadball$)|( rebound offensivedeadball$)|Deadball Rebound|deadball',
          desc
        ),
        team_rebound = grepl(
          'rebound offensive team|rebound defensive team|TEAM Offensive Rebound|TEAM Defensive Rebound',
          desc
        ),
        # turnover descriptors
        turnover = grepl(' turnover| Turnover',
                         desc),
        bad_pass_turnover = grepl(' bad pass',
                                  desc),
        lost_ball_turnover = grepl(' lost ball',
                                   desc),
        travel = grepl(' travel',
                       desc),
        dribbling_violation = grepl(' dribbling violation',
                                    desc),
        three_second_violation = grepl(' 3 seconds',
                                       desc),
        offensive_goaltending = grepl('offensive goal tending',
                                      desc),
        lane_violation = grepl(' lane violation',
                               desc),
        out_of_bounds = grepl(' out of bounds',
                              desc),
        # other plays
        steal = grepl('steal| Steal',
                      desc),
        jump_ball = grepl(' jumpball',
                          desc),
        timeout = grepl(' timeout', desc),
        substitution = grepl(' substitution| Enters Game| Leaves Game', desc),
        substitution_in = grepl(' substitution in| Enters Game', desc),
        substitution_out = grepl(' substitution out| Leaves Game', desc),
        points_on_play = free_throw_made + 2 * two_point_made + 3 * three_point_made,
        away_score = cumsum(points_on_play * (X4 == "")),
        home_score = cumsum(points_on_play * (X2 == "")),
        total = away_score + home_score,
        potential_points_on_play = as.integer(
          stringi::stri_count_regex(desc, '2pt| Two Point| Layup| Tip In| Dunk') * 2 + stringi::stri_count_regex(desc, '3pt| Three Point') * 3 + stringi::stri_count_regex(desc, ' freethrow| Free Throw')
        ),
        # extract player on play for lineups and joining
        player_on_play = trimws(
          dplyr::case_when(
            pbp_version == 'V2' ~ gsub(',', '', stringi::stri_extract_all_regex(desc, '(.*),')),
            pbp_version == 'V1' &
              pbp_version == 'V1' &
              stringi::stri_detect_regex(
                trimws(desc),
                '(\\b[A-Z\\\'\\.]+ \\b[A-Z\\\'\\.]+,\\b[A-Z\\\'\\.]+ )'
              ) ~ stringi::stri_extract_first_regex(
                trimws(desc),
                '(\\b[A-Z\\\'\\.]+ \\b[A-Z\\\'\\.]+,\\b[A-Z\\\'\\.]+ )'
              ),
            pbp_version == 'V1' &
              stringi::stri_detect_regex(trimws(desc), '(\\b[A-Z\\\'\\.]+,\\b[A-Z\\\'\\.]+ )') ~ stringi::stri_extract_first_regex(trimws(desc), '(\\b[A-Z\\\'\\.]+,\\b[A-Z\\\'\\.]+ )'),
            pbp_version == 'V1' &
              stringi::stri_detect_regex(
                trimws(desc),
                '(\\b[A-Z\\\'\\.]+ \\b[A-Z\\\'\\.]+ \\b[A-Z\\\'\\.]+ )'
              ) ~ stringi::stri_extract_first_regex(
                trimws(desc),
                '\\b[A-Z\\\'\\.]+ \\b[A-Z\\\'\\.]+ \\b[A-Z\\\'\\.]+ '
              ),
            T ~  stringi::stri_extract_first_regex(
              trimws(desc),
              '(\\b[A-Z][A-z\\\'\\.]+ \\b[A-Z][A-z\\\'\\.]+ )'
            )
          )
        ),
        player_on_play = trimws(
          dplyr::case_when(
            stringi::stri_detect_regex(toupper(trimws(player_on_play)), '^TEAM') ~ NA_character_,
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
        player_on_play = stringi::stri_trans_general(player_on_play, id = "Latin-ASCII"),
        # add a flag whenever a player joins or leaves the game
        on_court_flag = dplyr::case_when(substitution_in ~ 1,
                                         substitution_out ~ 0,
                                         T ~ NA_real_),
        # ordering = dplyr::case_when(
        #   timeout ~ 1,
        #   turnover ~ 2,
        #   field_goal_attempt ~ 3,
        #   assist ~ 4,
        #   foul ~ 5,
        #   substitution ~ 6,
        #   free_throw_attempt ~ 7,
        #   rebound ~ 8,
        #   T ~ 0
        # ),
      ) |>
      # dplyr::arrange(total) |>
      # # we do this odd ordering bit to make the substitutions a little easier
      # dplyr::arrange(period_second,
      #                period_first,
      #                desc(minute),
      #                desc(second),
      #                desc(centiseconds),
      #                ordering) |>
      dplyr::group_by(time) |>
      dplyr::mutate(has_fts = any(free_throw_attempt)) |>
      dplyr::ungroup() |>
      dplyr::filter(!(has_fts & dead_ball_rebound)) |>
      #' grouping by period because some lineups will change between periods
      #' without being noted in the pbp
      dplyr::group_by(period) |>
      dplyr::group_modify( ~ (
        \(a) {
          cbind(
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
                \(d) {
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
                }
              )()
          )
        }
      )(.x)) |>
      dplyr::ungroup() |>
      # we don't need any substitution data anymore, buhbye!
      dplyr::filter(!substitution) |>
      dplyr::mutate(on_court_string = on_court,
                    clean_play_check = stringi::stri_count_regex(on_court_string, ",") == 9) |> # ensures that we have 10 players on the floor for any given play
      # separating the lineup string -- this should _only_ be 10 players
      tidyr::separate(col = 'on_court',
                      sep = ',',
                      into = paste0('player_', c(1:10))) |>
      #' now we map player names to the roster df. this _could_ be noisy in theory
      #' but i haven't seen any issues in practice
      dplyr::mutate(
        # any time we get messy pbp data and are unsure of who is on court, NA these values
        across(dplyr::matches('player_([0-9]*)$'), ~ dplyr::case_when(!clean_play_check ~ '',
                                                                      T ~ .x)),
        across(dplyr::matches('player_([0-9]*)$'), ~ gsub('x_', '', .)),
        across(
          dplyr::matches('player_([0-9]*)$'),
          ~ (\(x){
            df <- data.frame(player_orig = x, row_number = dplyr::row_number())
            df |>
              fuzzyjoin::stringdist_left_join(rosters, by = c("player_orig"="player"), max_dist = 3, distance_col = "dist") |>
              dplyr::group_by(row_number) |>
              dplyr::arrange(row_number, dist) |>
              dplyr::summarize(player_id = dplyr::first(player_id)) |>
              dplyr::pull(player_id)
          })(.x),
          .names = '{.col}_id'
        ),
        across(
          dplyr::matches('player_([0-9]*)$'),
          ~ (\(x){
            df <- data.frame(player_orig = x, row_number = dplyr::row_number())
            df |>
              fuzzyjoin::stringdist_left_join(rosters, by = c("player_orig"="player"), max_dist = 3, distance_col = "dist") |>
              dplyr::group_by(row_number) |>
              dplyr::arrange(row_number, dist) |>
              dplyr::summarize(team_id = dplyr::first(team_id)) |>
              dplyr::pull(team_id)
          })(.x),
          .names = '{.col}_team_id'
        ),
        across(
          dplyr::matches('player_([0-9]*)$'),
          ~ (\(x){
            df <- data.frame(player_orig = x, row_number = dplyr::row_number())
            df |>
              fuzzyjoin::stringdist_left_join(rosters, by = c("player_orig"="player"), max_dist = 3, distance_col = "dist") |>
              dplyr::group_by(row_number) |>
              dplyr::arrange(row_number, dist) |>
              dplyr::summarize(pos = dplyr::first(pos)) |>
              dplyr::pull(pos)
          })(.x),
          .names = '{.col}_pos'
        ),
        player_on_play = dplyr::case_when(is.na(player_on_play) ~ "",
                                          T ~ player_on_play),
        player_on_play_id = (\(x){
          df <- data.frame(player_orig = x, row_number = dplyr::row_number())
          df |>
            fuzzyjoin::stringdist_left_join(rosters, by = c("player_orig"="player"), max_dist = 3, distance_col = "dist") |>
            dplyr::group_by(row_number) |>
            dplyr::arrange(row_number, dist) |>
            dplyr::summarize(player_id = dplyr::first(player_id)) |>
            dplyr::pull(player_id)
        })(player_on_play),
        player_on_play_team_id = dplyr::case_when(X2 == "" ~ away_team_id,
                                                  X4 == "" ~ home_team_id,
                                                  T ~ NA_character_),
        player_on_play_pos = (\(x){
          df <- data.frame(player_orig = x, row_number = dplyr::row_number())
          df |>
            fuzzyjoin::stringdist_left_join(rosters, by = c("player_orig"="player"), max_dist = 3, distance_col = "dist") |>
            dplyr::group_by(row_number) |>
            dplyr::arrange(row_number, dist) |>
            dplyr::summarize(pos = dplyr::first(pos)) |>
            dplyr::pull(pos)
        })(player_on_play),
      ) |>
      #' some character encoding stuff, dropping players with mispelled names in
      #' the pbp
      dplyr::mutate(
        dplyr::across(
          dplyr::matches("player_([0-9]*)$"),
          ~ dplyr::case_when(.x == "" ~ NA_character_,
                             T ~ .x)
        ),
        player_on_play = dplyr::case_when(player_on_play == "" ~ NA_character_,
                                          T ~ player_on_play),
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
                                          T ~ as.integer(points_on_play)),
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
        clean_play_check,
        on_court_string,
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
      ) |>
      dplyr::filter(trimws(desc) != 'null Turnover')
  })
  return(pbp)
}

#' @title ncaa_bb_pbp
#' @description Scrapes and parses pbp data for ncaa basketball games
#'
#' @param game_id the game_id for the specified game from stats.ncaa.com
#' @param dirty_data_behavior specifies the behavior for the function if
#' inconsistencies are detected between the box score and the pbp.
#' * If "error", then when an inconsistency is detected, the function will throw
#'   an error and not return anything.
#' * If "warn", then a warning will display but the function will still return
#'   pbp data.
#' * If "silent", the function will still check that the data is clean, but
#'   will not display any warning or error message.
#' This param defaults to "warn".
#'
#' @return a dataframe of cleaned, parsed play-by-play data for the specified
#' `game_id`
ncaa_bb_pbp <- function(game_id, dirty_data_behavior = "warn") {
  if (!(dirty_data_behavior %in% c("warn", "error", "silent"))) {
    cli::cli_abort(
      'Please specify a valid behavior for `dirty_data_behavior`, one of "error","warn", or "silent".'
    )
  }
  pbp_html <- ncaa_bb_pbp_html(game_id)
  box_html <- ncaa_bb_box_html(game_id)
  pbp <- ncaa_bb_pbp_parse(box_html, pbp_html)
  pbp_box <- ncaa_bb_calc_team_box_from_pbp(pbp)
  team_box <- ncaa_bb_team_box_parse(box_html) |>
    dplyr::select(c(
      team_id,
      fgm,
      fga,
      tpm,
      tpa,
      ftm,
      fta,
      pts,
      oreb,
      dreb,
      treb,
      ast,
      to,
      stl,
      blk
    )) |>
    dplyr::arrange(team_id)
  pbp$clean_game_check <- all(pbp_box == team_box)
  if (dirty_data_behavior == "warn" & !all(pbp_box == team_box)) {
    cli::cli_alert_danger(
      "Warning: inconsistencies detected between box score and PBP data for game_id {game_id}!"
    )
  } else if (dirty_data_behavior == "error" &
             !all(pbp_box == team_box)) {
    cli::cli_abort("Inconsistencies detected between box score and PBP data for game_id {game_id}!")
  }
  return(pbp)
}

#' @title ncaa_bb_player_box
#'
#' @description Obtains player box score data for a given basketball game_id
#' from stats.ncaa.org.
#'
#' @param game_id the game_id for a given game on stats.ncaa.org
#' @return a dataframe for box score data for that game
ncaa_bb_player_box <- function(game_id) {
  box_html <- ncaa_bb_box_html(game_id)
  player_box <- ncaa_bb_player_box_parse(box_html)
  return(player_box)
}

#' @title get_bb_team_box
#'
#' @description Obtains team box score data for a given basketball game_id from
#' stats.ncaa.org.
#'
#' @param game_id the game_id for a given game on stats.ncaa.org
#' @return a dataframe for box score data for that game
ncaa_bb_team_box <- function(game_id) {
  box_html <- ncaa_bb_box_html(game_id)
  team_box <- ncaa_bb_team_box_parse(box_html)
  return(team_box)
}
